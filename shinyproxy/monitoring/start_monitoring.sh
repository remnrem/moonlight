#!/bin/bash

# Prometheus monitoring system
cd /root/Programme/prometheus-2.41.0.linux-amd64
./prometheus --web.listen-address=0.0.0.0:7070 2>&1

# Export machine metrics
/root/Programme/node_exporter-1.5.0.linux-amd64/node_exporter 2>&1
