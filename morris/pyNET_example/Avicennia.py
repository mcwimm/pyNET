#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
List containing species specific plant model parameters.
@date: 2018 - Today
@author: jasper.bathmann@ufz.de
"""


def createPlant():
    geometry = {}
    parameter = {}
    # plant module BETTINA
    geometry["h_root"] = 0.0025
    geometry["h_crown"] = 0.004
    geometry["r_root"] = 0.2
    geometry["r_stem"] = 0.005
    geometry["h_stem"] = 0.05
    geometry["r_crown"] = 0.2
    parameter["leaf_water_potential"] = -7.86e6
    parameter["kf_sap"] = 1e-10
    parameter["lp"] = 0.33e-14
    parameter["k_geom"] = 4000
    parameter["half_max_h_growth_weight"] = 0.12
    parameter["maint_factor"] = 2e-7
    parameter["sun_c"] = 2.5e-8
    parameter["growth_factor"] = 0.0047
    parameter["h_sigmo_slope"] = 0.5
    parameter["sigmo_slope"] = 0.015

    return geometry, parameter
