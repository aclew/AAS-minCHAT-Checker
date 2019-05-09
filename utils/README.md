# eaf2txt


## EXTREMELY UGLY FIX

So that the reticulate package could import the Elan class (pympi is not available on conda), it was added directly INSIDE the eaf2txt script (!!!). It looks like local imports do not work with reticulate either, so it could not be a file on its own WAIT it could if it is sourced before probably?

## TODO

- [ ] try to source a file called Elan.py containing the Elan class
