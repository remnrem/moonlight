## Docker Multi-Platform build notes

Lists all builder instances and the nodes for each instance
```
docker buildx ls
```
```
NAME/NODE       DRIVER/ENDPOINT STATUS  BUILDKIT PLATFORMS
default                         error            
desktop-linux * docker                           
  desktop-linux desktop-linux   running 20.10.24 linux/arm64, linux/amd64, linux/riscv64, linux/ppc64le, linux/s390x, linux/386, linux/arm/v7, linux/arm/v6
```
Each builder has one or more nodes associated with it. The current builder’s name is marked with a `*` in `NAME/NODE` and 
explicit node to build against for the target platform marked with a `*` in the `PLATFORMS` column.

#### 1) Enter the following command to create a new builder, which we’ll call mybuilder

```R
docker buildx create --name moonlightbuilder --use desktop-linux --bootstrap
```
#### 2) You can inspect the builder by entering
```R
docker buildx inspect moonlightbuilder
```
#### 3) You can also see what runtime platforms your current builder instance supports by running
```R
docker buildx inspect --bootstrap
```
#### 4) Now, you’ll jumpstart your multi-architecture build with the single docker buildx command shown below
```R
 docker buildx build --platform=linux/arm64,linux/amd64 --push --tag remnrem/moonlight:latest -f Dockerfile .

```
The docker `buildx build` subcommand has a number of flags which determine where the final image will be stored. By default, i.e. if none of the flags are specified, the resulting image will remain captive in docker’s internal build cache. This is unlike the regular `docker build` command which stores the resulting image in the local `docker images` list.

#### 5) We can check the image with the imagetools subcommand which confirms the architecture versions that are included in the image
```R
docker buildx imagetools inspect "remnrem/moonlight"
docker pull remnrem/moonlight
docker inspect --format "{{.Architecture}}" "remnrem/moonlight"
```
