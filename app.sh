#!/usr/bin/env bash
set -o nounset -o errexit -o pipefail

# Sets the name of the Shiny app to be started.
# For valid values see https://shiny.rstudio.com/reference/shiny/latest/runapp
APP_NAME="./"

# Make number of app instances configurable, via command line or project environment variables. E.g.:
# APP_INSTANCES=4 bash app.sh
APP_INSTANCES=${APP_INSTANCES:-1}

# Make nginx port configurable so that this script can be tested in a workspace like this:
# NGINX_PORT=9999 bash app.sh
NGINX_PORT=${NGINX_PORT:-8888}

# Maximum number of failures within 60 seconds for nginx to stop sending traffic to an app instance for 60 seconds.
# See http://nginx.org/en/docs/http/ngx_http_upstream_module.html
MAX_FAILS=5

# Install nginx if missing
if ! which nginx &>/dev/null; then
  echo "Installing nginx."
  echo "Use a Domino environment with nginx pre-installed for faster start-up time."
  sudo apt-get update
  sudo apt-get install -y nginx
fi

# Give current user access to nginx directories
sudo chown -R $(id -u):$(id -g) /var/lib/nginx
sudo chown -R $(id -u):$(id -g) /var/log/nginx

# Start app instances in the background, and create nginx upstream partial configuration file
# with one entry for each app instance.
echo "Starting $APP_INSTANCES app server instance(s) in the background."
rm -f /tmp/nginx-upstream-partial.conf
for (( i=1; i<=$APP_INSTANCES; i++ ))
do
  # Use a sequential, unique port number for each app instance
  APP_PORT=$((8000 + $i))
  # Configure nginx to use this app instance
  echo "        server localhost:$APP_PORT max_fails=$MAX_FAILS fail_timeout=60s;" >>/tmp/nginx-upstream-partial.conf
  # Start the app instance in the background (that's what the & character means)
  R -e "shiny::runApp(\"$APP_NAME\", port=$APP_PORT, host=\"0.0.0.0\")" &
done

# Create nginx server partial configuration file
cat <<EOF >/tmp/nginx-server-partial.conf
        listen 0.0.0.0:$NGINX_PORT;
EOF

# Start nginx reverse proxy
echo "Starting nginx."
nginx -c $(pwd)/nginx.conf