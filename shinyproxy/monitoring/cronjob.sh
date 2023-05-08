@reboot sh /root/scripts/start_monitoring.sh
@reboot systemctl restart shinyproxy
@reboot systemctl restart docker
@reboot systemctl restart nginx
