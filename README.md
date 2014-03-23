# Fipes, plumbings to share files

Fipes provides you a simple way to share files with your friends.

## Goals

  - Provides a small web application.
  - Easy to install, easy to launch, easy to use.
  - Doesn't keep any data about you or your files.
  - Free as in freedom.

## Getting Started

**ACHTUNG: if you want to hack, you should clone the `develop`
branch**. See `CONTRIBUTING.md` for more information.

### Requirements

  * [Erlang](http://www.erlang.org/download.html) (R15B or later)

### Install the project

    $ git clone https://github.com/tOkeshu/fipes.git
    $ cd fipes
    $ make

These commands should pull the Erlang dependencies.

### Configure Nginx

Here is a sample configuration for nginx (you will need **nginx 1.4** or later to
have WebSocket proxying):


    # /etc/nginx/sites-available/fipes.example.com
    server {
        listen 80;
        # Or the line below if you want https
        # listen 443 ssl;

        root /path/to/fipes/public;
        index index.html index.htm;

        server_name fipes.example.com;
        server_name_in_redirect off;

        # Uncomment the lines below if you want https
        # ssl_certificate     /path/to/fipes.crt;
        # ssl_certificate_key /path/to/fipes.key;
        # ssl_protocols       SSLv3 TLSv1 TLSv1.1 TLSv1.2;
        # ssl_ciphers         HIGH;

        location / {
            proxy_read_timeout 900;
            proxy_pass http://127.0.0.1:3473;
        }

        # We need to turn off the buffering for Server-Sent Events
        location /stats {
            proxy_buffering off;
            proxy_pass http://127.0.0.1:3473/stats;
        }

        # WebSocket proxying (requires nginx 1.4 or later)
        location ~ /fipes/([^/]+)$ {
            proxy_read_timeout 900;

            proxy_http_version 1.1;
            proxy_pass http://127.0.0.1:3473/fipes/$1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_set_header Host $host;
        }

        # Uncomment the lines below if you want to launch the js tests
        #
        # location /tests/ {
        #     proxy_pass http://127.0.0.1:3473/tests.html;
        # }
    }

    # You may want to uncomment the lines below to always redirect
    # http to https
    # server {
    #     listen 80;
    #     server_name fipes.example.com;
    #     # Redirect http to https
    #     rewrite ^ https://$server_name$request_uri? permanent;
    # }

Enable your site:

    $ sudo ln -s /etc/nginx/sites-available/fipes.example.com /etc/nginx/sites-enabled/fipes.example.com
    $ sudo /etc/init.d/nginx reload

### Start the server

    $ cd fipes
    $ make start # start the server as a daemon on port 3473

Then open a browser to http://fipes.example.com (where
`fipes.example.com` is your domain).

If you just want to test the application on your machine, just edit
your `/etc/hosts`:

    # /etc/hosts
    127.0.1.1	fipes.example.com

and launch the server with:

    $ make dev

## Bugs/Pitfalls

  * Fipes is not p2p. However no data is stored on the server
    **ever**. The data just pass through the server, that's all.

  * Reloading the page while you're in a Fipe will stops the browser
    from serving your files. This is normal as the JavaScript File
    objects are lost while refreshing the page. You'll have to offers
    these files again.

  * For now, anyone can enter a Fipe.

## Contribute

See `CONTRIBUTING.md` in this repository for the contributing guidelines.

## What about the name

Fipes is a [Portmanteau](http://en.wikipedia.org/wiki/Portmanteau)
word combined from the two words *file* and *pipes*. In other words,
Fipes can be seen as a *pipe* for *files*.

## License

Fipes is released under the terms of the
[GNU Affero General Public License v3](http://www.gnu.org/licenses/agpl-3.0.html)
or later.

