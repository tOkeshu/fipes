# Fipes, plumbings to share files

Fipes provides you a simple way to share files with your friends.

## Goals

  - Provides a small web application.
  - Easy to install, easy to launch, easy to use.
  - Doesn't keep any data about you or your files.
  - Free as in freedom.

## Getting Started

### Requirements

  * [Erlang](http://www.erlang.org/download.html) (R14B04 or later)
  * [Rebar](https://github.com/basho/rebar) (already in the repository)

### Install the project

    $ git clone git://github.com/rgauthier/fipes.git
    $ cd fipes
    $ make app

These commands should pull the Erlang dependencies via Rebar and build
a release.

### Configure Nginx

Here is a sample configuration for nginx:

    # /etc/nginx/sites-available/fipes.example.com
    server {
        listen   80;

        root /path/to/fipes/public;
        index index.html index.htm;

        server_name fipes.example.com;
        server_name_in_redirect off;

        location / {
               proxy_pass http://127.0.0.1:3473/index.html;
        }

        location /fipes {
               proxy_pass http://127.0.0.1:3473/fipes;
        }

        location /static/ {
               proxy_pass http://127.0.0.1:3473/static/;
        }
    }

### Start the server

    $ cd fipes
    $ ./rel/fipes/bin/fipes start # start the server on port 3473

Then open a browser to http://fipes.example.com (where
`fipes.example.com` is your domain).

If you just want to test the application on your machine, just edit
your `/etc/hosts`:

    # /etc/hosts
    127.0.1.1	fipes.example.com

## Bugs/Pitfalls

  * For now, Fipes can't be used without Nginx proxying the application on
    port 3473 (as shown in the sample file). Any other configuration will
    probably fail.

  * Reloading the page while you're in a Fipe will stops the browser
    from serving your files. This is normal as the JavaScript File
    objects are lost while refreshing the page. You'll have to offers
    these files again.

  * For now, anyone can enter a Fipe.

## What about the name

Fipes is a [Portmanteau](http://en.wikipedia.org/wiki/Portmanteau)
word combined from the two words *file* and *pipes*. In other words,
Fipes can be seen as a *pipe* for *files*.

## License

Fipes is released under the terms of the
[GNU Affero General Public License v3](http://www.gnu.org/licenses/agpl-3.0.html)
or later.

