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

params = msample.sample(problem=problem,
                        N=50,
                        num_levels=6,
                        optimal_trajectories=10,
                        local_optimization=True,
                        seed=52343)

np.savetxt("Trasnfer/param_values.txt", params)
exit()
y = np.random.rand(len(params))

print(len(params))
exit()
analysis = manalyze.analyze(problem, params, y, conf_level=0.95,
                            print_to_console=True, num_levels=4)
