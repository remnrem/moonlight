# Automate the tagging and versioning process of your images being built
set -ex

USER='remnrem'
SERVICENAME='moonlight'

# Semantic versioning format (major_version.minor_version.patch)
version='1.0.0'

echo "version: $version"

docker build -t $USER/$SERVICENAME:$version .
