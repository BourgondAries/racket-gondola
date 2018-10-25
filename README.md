## Gondola Archive ##

To run this server:

1. `git clone https://github.com/BourgondAries/racket-gondola`
2. Install docker on your platform and make sure `docker run hello-world` works
3. `cd` to the cloned directory and run `./dock`. Pick yes to build the image.
4. The server will (after a little time) be active on port 8002

## Configuration ##

### Server Port ###
The `-p 8002:8000` clause in the file `dock` specifies which port the server is hosted on. Change 8002 to any other port to take effect.

### Disqus and Site words ###
`settings.rkt` contains various string settings, such as the disqus forum, the name of the site, the name of an entry, and so on.

### Logs ###
Logs are located in htdocs/logs.

### Videos ###
Videos are to be put inside htdocs/video/
To attach a source to a video, add the exact same filename (including extension) into htdocs/sources/.
htdocs/statistics are automatically generated when users access a video.

### /list (YourSite.com/list) ###
/list is the list of all videos with sources and views. It's regenerated every 1200 seconds, from within `handler.rkt`.
So if you add a video to htdocs/video, it will eventually show up on /list.

If you do not want to wait, you can `touch handler.rkt` to regenerate the list on-demand. The way this works is that `touch` causes the hot code reloader to reload the file, which resets all timers, and thus causes the list to be rebuilt.

### Extra SSL ###
Settings are located in main.rkt, specifically `#:ssl? #f`. If you change this to #t, put SSL keys as `private-key.pem` and `server-cert.pem` in the same directory as `main.rkt`.
If you have no SSL certificates, set `#:ssl? #f`.

Note that you ought to use a reverse proxy via nginx or apache if you can, and let that handle ssl certificates instead. But it is possible to run the server without SSL.

## The Server Crashes! ##

Check the log messages. If there's nothing interesting there, try increasing `-m=190` in `dock`. This limits the amount of memory usage. It's currently capped at 190 MB, but it might be too little for your architecture.

