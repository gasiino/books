#!/bin/sh -x
python setup.py build build_ext --inplace

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop)'
           