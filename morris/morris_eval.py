from SALib.sample import morris as msample
from SALib.analyze import morris as manalyze
import numpy as np

problem = {
    'num_vars': 7,
    'names': ['f_radius',
              'salinity', 'amplitude', 'deviation',
              'f_growth',
              'k_die', 'recruits'],
    'bounds': [[0.05, 0.95],
               [0, 70], [0, 15], [0, 5],
               [0.05, 0.95],
               [10**-14, 10**-12], [1, 30]]
}

params = np.loadtxt("Transfer/param_values.txt")
out_vars = ["no_trees", "dbh_mean", "root_mean", "fgraft", "gsx",
            "gsm", "no_groups", "i"]
timesteps = [400, 500, 600]
for o in range(0, len(out_vars)-1):
    for ts in timesteps:
        print(out_vars[o], ts)
        fn = "Transfer/pyOut_" + str(ts) + ".txt"

        Y = np.loadtxt(fn)
        Y = Y[:, o].flatten()
        print(Y)
        analysis = manalyze.analyze(problem, params, Y,
                                    conf_level=0.95,
                                    print_to_console=True, num_levels=6)
        res = [analysis["mu"], analysis["mu_star"],
               analysis["sigma"], analysis["mu_star_conf"]]

        np.savetxt("Results/morris_" + str(out_vars[o]) + "_" + str(ts) + ".txt", res)
