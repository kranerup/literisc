`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 11/23/2024 07:29:05 PM
// Design Name: 
// Module Name: blinky2
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module blinky2(
    input sysclk,
    output [1:0] led,
    input uart_txd_in,
    output uart_rxd_out
    );
wire clk10;


wire uart_tx;
assign uart_rxd_out = uart_tx;
wire uart_rx;
assign uart_rx = uart_txd_in;


wire locked;
reg reset = 0;
reg sync_rstn = 0;

always_ff @(posedge clk10) begin
  if ( locked ) begin
     sync_rstn <= 1;
  end
end

clk_wiz_0 i_clkwiz(
  .reset(reset),
  .clk_in1( sysclk ),
  .clk10( clk10 ),
  .locked( locked ));

// 19,200 * 16 = 307,200
// 10 * 10^6 / 307,200 = ~33      (counter limit M)
// log2(33) = 6                  (counter bits N)
localparam        DBITS = 8;          // number of data bits in a word
localparam        SB_TICK = 16;       // number of stop bit / oversampling ticks
localparam        BR_LIMIT = 33;     // baud rate generator counter limit
localparam        BR_BITS = 6;       // number of baud rate generator counter bits

wire tick;                  
wire ser_reset;
assign ser_reset = ~sync_rstn;
wire [7:0] tx_data;
wire [7:0] rx_data;
wire ser_send;
wire rx_ack;
wire rx_ready;
wire tx_ready;



    baud_rate_generator                                                                         
        #(
            .M(BR_LIMIT), 
            .N(BR_BITS)
         ) 
        BAUD_RATE_GEN   
        (
            .clk_100MHz(clk10),                                                                   
            .reset(ser_reset),
            .tick(tick)
         );

    uart_transmitter                                                                            
        #(
            .DBITS(DBITS),
            .SB_TICK(SB_TICK)
         )
         UART_TX_UNIT                                                                           
         (
            .clk_100MHz(clk10),
            .reset(ser_reset),
            .tx_start(ser_send),
            .sample_tick(tick),                                                                 
            .data_in(tx_data),
            .tx_ready(tx_ready),
            .tx(uart_tx)
         );

    uart_receiver
        #(
            .DBITS(DBITS),
            .SB_TICK(SB_TICK)
         )
         UART_RX_UNIT
         (
            .clk_100MHz(clk10),
            .reset(ser_reset),
            .rx(uart_rx),
            .sample_tick(tick),
            .data_ready(rx_ready),
            .data_ack(rx_ack),
            .data_out(rx_data)
         );


// -------------- CPU -----------------------------------
wire [31:0] gpio;
assign led[0] = gpio[20];
assign led[1] = gpio[21];

soc isoc(
    .clk( clk10 ),
    .sync_rstn( sync_rstn ),
    .gpio( gpio ),
    .serial_tx_data( tx_data ),
    .serial_tx_send( ser_send ),
    .serial_tx_ready( tx_ready ),
    .serial_rx_data( rx_data ),
    .serial_rx_ready( rx_ready ),
    .serial_rx_ack( rx_ack )
    );

`ifdef NONE
uart_tx_ctrl i_uart(
  .SEND( ser_send ),
  .DATA( tx_data ),
  .CLK( clk10 ),
  .READY( uart_ready ),
  .UART_TX( uart_tx ));
`endif

endmodule
