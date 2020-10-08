"""
Fit the model using limetr
"""
from pathlib import Path
import numpy as np
import pandas as pd
from limetr import LimeTr


# settings
num_trials = 30
data_folder = Path("./data")
results_folder = Path("./results")


# def functions
def fit_data(data: pd.DataFrame,
             inlier_percentage: float = 0.8,
             use_obs_se: bool = True):
    data.sort_values("study_id", inplace=True)
    data.reset_index(inplace=True)

    # parameters
    num_obs = data.shape[0]
    _, study_sizes = np.unique(data.study_id, return_counts=True)
    k_beta = 2
    k_gamma = 1

    # create covariates and functions
    x_mat = np.vstack((np.ones(num_obs), data.x1.values)).T
    z_mat = np.ones((num_obs, 1))

    def x_fun(beta): return x_mat.dot(beta)
    def x_jac_fun(beta): return x_mat

    obs = data.obs.values
    obs_sd = data.obs_sd.values

    # create limetr object
    if use_obs_se:
        lt = LimeTr(
            study_sizes,
            k_beta,
            k_gamma,
            obs,
            x_fun,
            x_jac_fun,
            z_mat,
            inlier_percentage=inlier_percentage,
            S=obs_sd
        )
    else:
        lt = LimeTr(
            study_sizes,
            k_beta,
            k_gamma,
            obs,
            x_fun,
            x_jac_fun,
            z_mat,
            inlier_percentage=inlier_percentage,
            share_obs_std=True
        )

    beta_soln, gamma_soln, w_soln = lt.fitModel(
        outer_max_iter=100000,
        outer_step_size=200.0
    )

    df_result = pd.DataFrame({
        'model': ['LimeTr'],
        'beta0': beta_soln[0],
        'beta1': beta_soln[1],
        'gamma': gamma_soln[0],
        'true_outlier_detected': sum((lt.w == 0) & (data.outliers != 0)),
        'sample_size': num_obs
    })

    if not use_obs_se:
        df_result.insert(loc=4, column="delta", value=lt.delta)

    return df_result


if __name__ == '__main__':
    # load data
    dfs = [pd.read_csv(data_folder / f"data_{(i+1)}.csv")
           for i in range(num_trials)]

    case_meta = pd.concat([fit_data(df) for df in dfs])
    case_meta.insert(loc=0, column="data_id", value=np.arange(1, num_trials + 1))
    case_meta.to_csv(results_folder / "case_meta.csv", index=False)

    case_long = pd.concat([fit_data(df, use_obs_se=False) for df in dfs])
    case_long.insert(loc=0, column="data_id", value=np.arange(1, num_trials + 1))
    case_long.to_csv(results_folder / "case_long.csv", index=False)
