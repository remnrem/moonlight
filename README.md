
# Moonlight viewer

## build notes

```
 docker buildx create --name mybuilder --platform linux/arm64,linux/amd64
 docker buildx use mybuilder
 docker buildx inspect mybuilder
 docker buildx inspect --bootstrap
 docker buildx build --platform=linux/arm64,linux/amd64 --push --tag remnrem/moonlight:latest .
```

```
 docker run --rm -it -p 3838:3838 -v=.:/data/ remnrem/moonlight:latest
 docker run --rm -it -p 3838:3838 -v=.:/data/ moonlight1 
```
## TODO / Known issues

 - when small regions of hypnogram selected (to define lights off/on),
   will give an error calculating hyonogram statistics if not
   sufficient number of stages
  
