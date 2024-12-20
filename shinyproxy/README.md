![](shinyproxy.png)

![](https://img.shields.io/badge/Platform-Ubuntu--64%20-blue.svg)
![](https://img.shields.io/badge/ShinyProxy-2.6.1%20-blue.svg)
![](https://img.shields.io/badge/Docker-20.10.22%20-blue.svg)
![](https://img.shields.io/badge/OpenJDK_Zulu-8%20-blue.svg)


# ShinyProxy
ShinyProxy is your favourite way to deploy Shiny apps in an enterprise context.<br />
When deploying a Shiny application with ShinyProxy, the application is simply bundled 
as an R package and installed into a Docker image.<br /> Every time a user runs an application, 
a container spins up and serves the application.


# Installation

## 1) Java 8
ShinyProxy is written using mature and robust Java technology and you will need a Java 8 runtime environment to run ShinyProxy
#### Download and Install [OpenJDK like Zulu](https://www.azul.com/downloads/?package=jdk)
* Select Java version 8 LTS
```
sudo apt install ./zulu8.68.0.19-ca-jdk8.0.362-linux_amd64.deb
```
To check your version of Java, run
```
java -version
```
which should display something along
```
openjdk version "1.8.0_362"
OpenJDK Runtime Environment (Zulu 8.68.0.19-CA-linux64) (build 1.8.0_362-b08)
OpenJDK 64-Bit Server VM (Zulu 8.68.0.19-CA-linux64) (build 25.362-b08, mixed mode)
```
## 2) Docker

#### Download and Install [Docker for ubuntu](https://docs.docker.com/engine/install/ubuntu/)
Once the installation is completed, To check whether the docker daemon is up and running, use
```
sudo systemctl status docker
```
ShinyProxy needs to connect to the docker daemon to spin up the containers for the Shiny apps.<br /> 
By default ShinyProxy will do so on port 2375 of the docker host.<br /> In order to allow for connections on port 2375, 
the startup options need to be edited.<br /> Edit `/lib/systemd/system/docker.service` and replace the relevant line with
```
ExecStart=/usr/bin/dockerd -H unix:// -D -H tcp://127.0.0.1:2375
```
Next, save the file, close the editor and restart Docker using:
```
sudo systemctl restart docker
```

## 3) ShinyProxy
#### Download and Install [ShinyProxy](https://www.shinyproxy.io/downloads/)
 ```
 sudo apt install ./shinyproxy_2.6.1_amd64.deb
 ```

## 4) Pulling the demo image 
In order to run ShinyProxy, you need Shiny apps. In ShinyProxy such Shiny apps are typically shipped in docker containers and the `openanalytics/shinyproxy-demo` is a demo image that has been made available to start playing with Shiny Proxy.

Once docker is installed on your system, you can pull (i.e. download) the docker image with the demo applications using
```
sudo docker pull openanalytics/shinyproxy-demo
```
## 5) ShinyProxy configuration
ShinyProxy looks for a [configuration](https://www.shinyproxy.io/documentation/configuration/) file called `application.yml` in the `/etc/shinyproxy directory`. Let's create this file:
```
touch /etc/shinyproxy/application.yml
```
Copy the below configs to `application.yml`
```python
proxy:
    title:
    landing-page: /
    heartbeat-rate: 15000
    heartbeat-timeout: 900000
    port: 9191
    container-wait-time: 800000
    container-log-path: ./container-logs
  
    authentication: none

    docker:
      url: http://localhost:2375
      port-range-start: 20000

    specs:
    - id: euler
      display-name: Euler’s number
      container-cmd: ["R", "-e", "shiny::runApp('/root/euler')"]
      container-image: openanalytics/shinyproxy-template
 ```
 ## 6) Running ShinyProxy 
Restart ShinyProxy daemon
```
sudo systemctl restart shinyproxy
```
To see whether the service is up and running, one can use:
```
sudo systemctl status shinyproxy
```
Less than 10 seconds later, you can point your browser to http://localhost:9191 and use your Shiny apps!<br />
More advanced information on the usage and configuration of ShinyProxy is available on the [Configuration](https://www.shinyproxy.io/documentation/configuration/) page <br />
and [Openanalytics Github](https://github.com/openanalytics/shinyproxy-config-examples) page.
