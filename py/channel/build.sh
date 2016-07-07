python setup.py build_ext --inplace
rm -rf build src/pyx/channel.c
strip channel.so
python test_channel.py
