module vlog_soc_tb;

initial
begin
   $dumpfile("vlog.fst");
   $dumpvars(0,vlog_soc_tb);
end

reg clk = 0;
reg [0:0] sync_rstn;
reg [31:0] gpio;
reg [7:0] serial_tx_data;
reg [7:0] serial_rx_data;
reg serial_tx_send;
reg serial_tx_ready;
reg serial_rx_ready;
reg uart_tx;

// 19,200 * 16 = 307,200
// 10 * 10^6 / 307,200 = ~33      (counter limit M)
// log2(33) = 6                  (counter bits N)
localparam        DBITS = 8;          // number of data bits in a word
localparam        SB_TICK = 16;       // number of stop bit / oversampling ticks
localparam        BR_LIMIT = 33;     // baud rate generator counter limit
localparam        BR_BITS = 6;       // number of baud rate generator counter bits

wire reset;
assign reset = ~sync_rstn;
wire tick;

    baud_rate_generator 
        #(
            .M(BR_LIMIT), 
            .N(BR_BITS)
         ) 
        BAUD_RATE_GEN   
        (
            .clk_100MHz(clk), 
            .reset(reset),
            .tick(tick)
         );
    
    uart_transmitter
        #(
            .DBITS(DBITS),
            .SB_TICK(SB_TICK)
         )
         UART_TX_UNIT
         (
            .clk_100MHz(clk),
            .reset(reset),
            .tx_start(serial_tx_send),
            .sample_tick(tick),
            .data_in(serial_tx_data),
            .tx_ready(serial_tx_ready),
            .tx(uart_tx)
         );

    uart_receiver
        #(
            .DBITS(DBITS),
            .SB_TICK(SB_TICK)
         )
         UART_RX_UNIT
         (
            .clk_100MHz(clk),
            .reset(reset),
            .rx(uart_tx),
            .sample_tick(tick),
            .data_ready(serial_rx_ready),
            .data_ack(serial_rx_ack),
            .data_out(serial_rx_data)
         );

soc soc(
    clk,
    sync_rstn,
    gpio, 
    serial_tx_data,
    serial_tx_send,
    serial_tx_ready,
    serial_rx_data,
    serial_rx_ready,
    serial_rx_ack);

  initial begin
      @(posedge clk);
      sync_rstn = 0;
      @(posedge clk);
      @(posedge clk);
      sync_rstn = 1;
      @(posedge clk);
  end

  initial begin
      #200000 $finish;
  end

  always #1 clk = !clk;

endmodule
