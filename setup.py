from setuptools import setup, find_packages
from codecs import open
from os import path

here = path.abspath(path.dirname(__file__))

with open(path.join(here, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()

setup(
    name='hpl_personal_python_library',

    version='0.0.0',

    description='Library containing python scripts for use by HPL',

    author='Harrison Pielke-Lombardo',

    author_email='harrison.pielke-lombardo@ucdenver.edu',

    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Personal',
        'License :: MIT License',
        'Programming Language :: Python :: 3.6'
    ],

    packages=find_packages(),

    install_requires=['ipywidgets', 'IPython']
)