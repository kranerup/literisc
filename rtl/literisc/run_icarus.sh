#!/usr/bin/env bash
iverilog -DSYNTHESIS -g2005-sv \
    uart/baud_rate_generator.v \
    uart/uart_transmitter.v  \
    uart/uart_receiver.v  \
    vlog_soc_tb.sv \
    soc.v \
    pa_cpu_3bd4d9.v && ./a.out  -fst


