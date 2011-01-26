
(defmodule dummy_ping
  (export (ping 0)))		;Just indicates intent

(defun ping ()
  (: 'pong))
