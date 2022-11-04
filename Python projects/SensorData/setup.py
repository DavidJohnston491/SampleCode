# -*- coding: utf-8 -*-
# Windows Python 3.2.py


#Create cx-Freeze setup script

from cx_Freeze import setup, Executable

setup(
    name = "sensorDataParsing",
    version = "0.1",
    description = "Correct date and create csv files for Tony Nadelko",
    executables = [Executable("dataParse.py")])
