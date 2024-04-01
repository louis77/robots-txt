This package provides a ROBOTS.TXT parser. Its main function is 
to match a given clause against a ROBOTS.TXT file and tell if it
matches or not.

It adheres to the Gemini robots.txt community spec described here:
gemini://geminiprotocol.net/docs/companion/robots.gmi

Installation:

Either clone it into your ~/common-lisp directory or fetch from Ultralisp:
(ql:quickload :robots-txt)
 
Usage:

1. Create a DISALLOWED-TABLE with MAKE-DISALLOWED-TABLE providing the ROBOTS.TXT content as a string.

CL-USER > (defparameter *my-robots-txt* (robots-txt:make-disallowed-table "User-agent: researcher
User-agent: indexer
User-agent: archiver
Disallow: /v/search
Disallow: /search
Disallow: /backlinks

User-agent: *
Disallow: /add-seed"))

2. Query DISALLOWED-P with the DISALLOWED-TABLE and a User-Agent and Path:

CL-USER > (robots-txt:disallowed-p *my-robots-txt* "indexer" "/search?hello")
=> T

License: MIT
