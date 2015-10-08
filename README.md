# org-present-mode

This is meant to be an extremely minimalist presentation tool for
Emacs [org-mode](http://orgmode.org/).  Simply layout your
presentation with each slide under a top-level header, start the minor
mode with 'org-present', and page through each slide with left/right
keys.

## Philosophy

Most of the time I'm giving a talk, it is a work in progress and I want to be be able to edit
as I go along. Also, to split my frame and work on code examples with my slides still visible.

## Configuration

Add something like this to your emacs config:

```lisp
(add-to-list 'load-path "~/path/to/org-present")
(autoload 'org-present "org-present" nil t)
```

Precise behaviour of org-present during start and quit is controlled
from hooks. The following will enlarge text, show images, hide the
cursor and make the buffer read-only:

```lisp
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))
```

Then start the minor mode with:

```
M-x org-present
```

Keys are:
- left/right for movement
- C-c C-= for large txt
- C-c C-- for small text
- C-c C-q for quit (which will return you back to vanilla org-mode)
- C-c < and C-c > to jump to first/last slide

## Beautification

This works well with
[hide-mode-line](http://webonastick.com/emacs-lisp/hide-mode-line.el),
which hides the mode-line when only one frame and buffer are open.

If you're on a Mac with an older emacs you might also want to look at the
[fullscreen patch](http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch).
`toggle-frame-fullscreen` comes with emacs 24.

## Remote Control

org-present includes a simple, mobile-friendly web remote control for
moving between slides.  To start it, first enter presentation mode
with `M-x org-present`, then do `M-x org-present-remote-on`.

You will need to enter the host to which to make the interface
available on.  For local testing this can be `localhost`.  For actual
use you will need to supply your machine's IP address.

Then use a browser to open the remote control by browsing to port 8009
on the host you supplied, e.g.:

[http://localhost:8009/](http://localhost:8009)

### How do I determine my IP address?
On many *NIX systems you can do this with ifconfig.  For example, to
determine the IP address of your WiFi adaptor on Linux Mint:
```
$ ifconfig wlan0
wlan0     Link encap:Ethernet  HWaddr f4:06:69:69:fd:4d
          UP BROADCAST MULTICAST  MTU:1500  Metric:1
          RX packets:0 errors:0 dropped:0 overruns:0 frame:0
          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000
          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)
```

## Copyright

Copyright © 2014 Richard Lister.
