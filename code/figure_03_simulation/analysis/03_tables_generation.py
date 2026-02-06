#!/usr/bin/env python3
# ==============================================================================
# 03_tables_generation.py — Generate all tables for the paper
# ==============================================================================
#
# Part of:  DPprior Replication Package (Simulation Analysis Pipeline)
# Pipeline: Step 3 of 3 — Tables Generation
# Depends:  01_data_preparation.py (preprocessed_results.csv)
# Produces: Tables in tables/main/ and tables/supplement/ (csv, md, tex)
#
# Table-to-Paper Mapping:
#   Code Output                      → Paper Table
#   ──────────────────────────────────────────────
#   table01_main_results             → Table 2 (Main Text)
#   table02_collapse_probability     → Table F1 (Appendix F)
#   table03_bias                     → Table F2 (Appendix F)
#   table03b_mae                     → Table F2b (Appendix F)
#   table04_mse                      → Table F3 (Appendix F)
#   table04b_rmse                    → Table F3b (Appendix F)
#   table05_coverage                 → Table F4 (Appendix F)
#   table05b_ci_width                → Table F4b (Appendix F)
#   table06_low_info_summary         → Table F5 (Appendix F)
#   table09_relative_improvement     → Table F6 (Appendix F)
#   table07_all_methods              → Table S1 (Online Supplement)
#   table08_confidence_comparison    → Table S2 (Online Supplement)
#   table10_alpha_posterior           → Table S3 (Online Supplement)
#   table11_prior_hyperparameters    → Table S4 (Online Supplement)
#   table12_exact_match              → Table S5 (Online Supplement)
#   table12b_near_match              → Table S5b (Online Supplement)
#
# Usage:
#   cd <repo-root>
#   python code/figure_03_simulation/analysis/03_tables_generation.py
#
# ==============================================================================

import pandas as pd
import numpy as np
import os
from pathlib import Path

# =============================================================================
# PATH CONFIGURATION
# =============================================================================

REPO_ROOT = Path(__file__).resolve().parents[3]
DATA_DIR = REPO_ROOT / "data" / "simulation_results"
TABLES_DIR = REPO_ROOT / "tables"

# For backward compatibility, also define subdirectory aliases
# (all tables now saved to flat tables/ directory)
TABLES_MAIN = TABLES_DIR
TABLES_SUPP = TABLES_DIR

# Create output directory if it doesn't exist
os.makedirs(str(TABLES_DIR), exist_ok=True)

# Load preprocessed data
df = pd.read_csv(str(DATA_DIR / "preprocessed_results.csv"))

# Method order
MAIN_METHODS = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor']
ALL_METHODS = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor', 'χ²-DORO']


def save_table(df_table, filepath, table_title="", table_note=""):
    """Save table in multiple formats: CSV, Markdown, LaTeX."""
    filepath_obj = Path(filepath)
    base_path = filepath_obj.parent / filepath_obj.stem
    
    # CSV
    df_table.to_csv(str(base_path) + ".csv", index=True)
    
    # Markdown
    with open(str(base_path) + ".md", 'w') as f:
        if table_title:
            f.write(f"# {table_title}\n\n")
        f.write(df_table.to_markdown())
        if table_note:
            f.write(f"\n\n*Note:* {table_note}")
    
    # LaTeX
    latex_str = df_table.to_latex(escape=False, multirow=True, multicolumn=True)
    with open(str(base_path) + ".tex", 'w') as f:
        if table_title:
            f.write(f"% {table_title}\n")
        f.write(latex_str)
        if table_note:
            f.write(f"\n% Note: {table_note}")
    
    print(f"✓ Saved: {base_path} (csv, md, tex)")


# =============================================================================
# TABLE 1: MAIN RESULTS (E[K], Bias, RMSE, P(K=1), Coverage)
# Paper Table 2 (Main Text)
# =============================================================================

def table01_main_results():
    """Generate main results table with posterior mean, bias, RMSE, collapse prob, coverage."""
    # E[K]
    table_ek = df.groupby(['K_true', 'I_target', 'method_label'])['post_EK'].agg(['mean', 'std']).reset_index()
    table_ek['E[K]'] = table_ek.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot_ek = table_ek.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='E[K]')
    pivot_ek = pivot_ek[MAIN_METHODS]
    
    # Bias
    table_bias = df.groupby(['K_true', 'I_target', 'method_label'])['bias_K'].agg(['mean', 'std']).reset_index()
    table_bias['Bias'] = table_bias.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot_bias = table_bias.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Bias')
    pivot_bias = pivot_bias[MAIN_METHODS]
    
    # RMSE
    table_mse = df.groupby(['K_true', 'I_target', 'method_label'])['mse_K'].mean().reset_index()
    table_mse['RMSE'] = np.sqrt(table_mse['mse_K']).apply(lambda x: f"{x:.3f}")
    pivot_rmse = table_mse.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='RMSE')
    pivot_rmse = pivot_rmse[MAIN_METHODS]
    
    # P(K=1)
    table_pkone = df.groupby(['K_true', 'I_target', 'method_label'])['post_probK1'].mean().reset_index()
    table_pkone['P(K=1)'] = table_pkone['post_probK1'].apply(lambda x: f"{x:.3f}")
    pivot_pkone = table_pkone.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='P(K=1)')
    pivot_pkone = pivot_pkone[MAIN_METHODS]
    
    # Coverage
    table_cov = df.groupby(['K_true', 'I_target', 'method_label'])['covers_true'].mean().reset_index()
    table_cov['Coverage'] = table_cov['covers_true'].apply(lambda x: f"{x:.3f}")
    pivot_cov = table_cov.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Coverage')
    pivot_cov = pivot_cov[MAIN_METHODS]
    
    # Combine tables
    combined = pd.concat([pivot_ek, pivot_bias, pivot_rmse, pivot_pkone, pivot_cov], axis=1)
    
    save_table(combined, str(TABLES_MAIN / "table01_main_results.csv"), 
               table_title="Main Results", 
               table_note="E[K] = Posterior Mean, Bias, RMSE, P(K=1), Coverage")


# =============================================================================
# TABLE 2: COLLAPSE PROBABILITY
# Paper Table F1 (Appendix F)
# =============================================================================

def table02_collapse_probability():
    """Generate collapse probability table P(K=1)."""
    table = df.groupby(['K_true', 'I_target', 'method_label'])['post_probK1'].agg(['mean', 'std']).reset_index()
    table['P(K=1)'] = table.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot = table.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='P(K=1)')
    pivot = pivot[MAIN_METHODS]
    
    save_table(pivot, str(TABLES_MAIN / "table02_collapse_probability.csv"), 
               table_title="Collapse Probability", 
               table_note="P(K=1) = Posterior Probability of Collapse")


# =============================================================================
# TABLE 3: BIAS (Mean and MAE)
# Paper Table F2 (Appendix F) and Table F2b (Appendix F)
# =============================================================================

def table03_bias_statistics():
    """Generate bias statistics table."""
    # Bias
    table_bias = df.groupby(['K_true', 'I_target', 'method_label'])['bias_K'].agg(['mean', 'std']).reset_index()
    table_bias['Bias'] = table_bias.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot_bias = table_bias.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Bias')
    pivot_bias = pivot_bias[MAIN_METHODS]
    
    save_table(pivot_bias, str(TABLES_MAIN / "table03_bias.csv"), 
               table_title="Bias Statistics", 
               table_note="Bias = Mean Estimate - True Value")
    
    # MAE
    table_mae = df.groupby(['K_true', 'I_target', 'method_label'])['abs_bias_K'].agg(['mean', 'std']).reset_index()
    table_mae['MAE'] = table_mae.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot_mae = table_mae.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='MAE')
    pivot_mae = pivot_mae[MAIN_METHODS]
    
    save_table(pivot_mae, str(TABLES_MAIN / "table03b_mae.csv"), 
               table_title="Mean Absolute Error", 
               table_note="MAE = Mean Absolute Error")


# =============================================================================
# TABLE 4: MSE AND RMSE
# Paper Table F3 (Appendix F) and Table F3b (Appendix F)
# =============================================================================

def table04_mse_rmse():
    """Generate MSE and RMSE tables."""
    # MSE
    table_mse = df.groupby(['K_true', 'I_target', 'method_label'])['mse_K'].mean().reset_index()
    table_mse['MSE'] = table_mse['mse_K'].apply(lambda x: f"{x:.4f}")
    pivot_mse = table_mse.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='MSE')
    pivot_mse = pivot_mse[MAIN_METHODS]
    
    save_table(pivot_mse, str(TABLES_MAIN / "table04_mse.csv"), 
               table_title="Mean Squared Error", 
               table_note="MSE = Mean Squared Error")
    
    # RMSE
    table_rmse = df.groupby(['K_true', 'I_target', 'method_label'])['mse_K'].mean().reset_index()
    table_rmse['RMSE'] = np.sqrt(table_rmse['mse_K']).apply(lambda x: f"{x:.3f}")
    pivot_rmse = table_rmse.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='RMSE')
    pivot_rmse = pivot_rmse[MAIN_METHODS]
    
    save_table(pivot_rmse, str(TABLES_MAIN / "table04b_rmse.csv"), 
               table_title="Root Mean Squared Error", 
               table_note="RMSE = sqrt(MSE)")


# =============================================================================
# TABLE 5: COVERAGE AND CI WIDTH
# Paper Table F4 (Appendix F) and Table F4b (Appendix F)
# =============================================================================

def table05_coverage_ci():
    """Generate coverage probability and CI width tables."""
    # Coverage
    table_cov = df.groupby(['K_true', 'I_target', 'method_label'])['covers_true'].mean().reset_index()
    table_cov['Coverage'] = table_cov['covers_true'].apply(lambda x: f"{x:.3f}")
    pivot_cov = table_cov.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Coverage')
    pivot_cov = pivot_cov[MAIN_METHODS]
    
    save_table(pivot_cov, str(TABLES_MAIN / "table05_coverage.csv"), 
               table_title="Coverage Probability", 
               table_note="Coverage = Proportion of Samples with True K in 95% CI")
    
    # CI Width
    table_width = df.groupby(['K_true', 'I_target', 'method_label'])['ci_width'].agg(['mean', 'std']).reset_index()
    table_width['CI_Width'] = table_width.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot_width = table_width.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='CI_Width')
    pivot_width = pivot_width[MAIN_METHODS]
    
    save_table(pivot_width, str(TABLES_MAIN / "table05b_ci_width.csv"), 
               table_title="Credible Interval Width", 
               table_note="CI_Width = Mean Width of 95% Credible Interval")


# =============================================================================
# TABLE 6: LOW INFORMATION SUMMARY
# Paper Table F5 (Appendix F)
# =============================================================================

def table06_low_info_summary():
    """Generate summary table for low information scenario (I_target=0.2)."""
    df_low = df[df['I_target'] == 0.2].copy()
    
    methods = MAIN_METHODS
    k_true_vals = sorted(df_low['K_true'].unique())
    
    rows = []
    for k in k_true_vals:
        df_k = df_low[df_low['K_true'] == k]
        row = {'K_true': f"K*={k}"}
        
        for method in methods:
            df_method = df_k[df_k['method_label'] == method]
            
            ek_mean = df_method['post_EK'].mean()
            bias = df_method['bias_K'].mean()
            rmse = np.sqrt(df_method['mse_K'].mean())
            pk1 = df_method['post_probK1'].mean()
            cov = df_method['covers_true'].mean()
            
            row[method] = (f"E[K]={ek_mean:.3f}, Bias={bias:.3f}, RMSE={rmse:.3f}, "
                           f"P(K=1)={pk1:.3f}, Cov={cov:.3f}")
        
        rows.append(row)
    
    table = pd.DataFrame(rows).set_index('K_true')
    
    save_table(table, str(TABLES_MAIN / "table06_low_info_summary.csv"), 
               table_title="Low Information Summary (I=0.2)", 
               table_note="Comprehensive summary of all metrics for low information scenario")


# =============================================================================
# TABLE 7: ALL METHODS INCLUDING χ²-DORO
# Paper Table S1 (Online Supplement)
# =============================================================================

def table07_all_methods():
    """Generate table with all methods including χ²-DORO."""
    table = df.groupby(['K_true', 'I_target', 'method_label'])['post_EK'].agg(['mean', 'std']).reset_index()
    table['E[K]'] = table.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot = table.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='E[K]')
    pivot = pivot[ALL_METHODS]
    
    save_table(pivot, str(TABLES_SUPP / "table07_all_methods.csv"), 
               table_title="All Methods Comparison", 
               table_note="Includes χ²-DORO baseline method")


# =============================================================================
# TABLE 8: CONFIDENCE LEVEL COMPARISON
# Paper Table S2 (Online Supplement)
# =============================================================================

def table08_confidence_comparison():
    """Generate table comparing TSMM and Dual-Anchor across confidence levels."""
    df_conf = df[df['method_label'].isin(['TSMM', 'Dual-Anchor'])].copy()
    
    table = df_conf.groupby(['K_true', 'I_target', 'method_label'])['post_EK'].agg(['mean', 'std']).reset_index()
    table['E[K]'] = table.apply(lambda x: f"{x['mean']:.3f} ({x['std']:.3f})", axis=1)
    pivot = table.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='E[K]')
    
    save_table(pivot, str(TABLES_SUPP / "table08_confidence_comparison.csv"), 
               table_title="Confidence Level Comparison", 
               table_note="TSMM and Dual-Anchor across information levels")


# =============================================================================
# TABLE 9: RELATIVE IMPROVEMENT OVER VAGUE
# Paper Table F6 (Appendix F)
# =============================================================================

def table09_relative_improvement():
    """Generate table with relative MSE improvement over Vague prior."""
    table_mse = df.groupby(['K_true', 'I_target', 'method_label'])['mse_K'].mean().reset_index()
    
    vague_mse = table_mse[table_mse['method_label'] == 'Vague'][['K_true', 'I_target', 'mse_K']].copy()
    vague_mse.rename(columns={'mse_K': 'vague_mse'}, inplace=True)
    
    table_rel = table_mse.merge(vague_mse, on=['K_true', 'I_target'])
    table_rel['rel_improvement'] = ((table_rel['vague_mse'] - table_rel['mse_K']) / table_rel['vague_mse'] * 100)
    table_rel['Relative Improvement %'] = table_rel['rel_improvement'].apply(lambda x: f"{x:.1f}%")
    
    pivot = table_rel.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Relative Improvement %')
    pivot = pivot[MAIN_METHODS]
    
    save_table(pivot, str(TABLES_MAIN / "table09_relative_improvement.csv"), 
               table_title="Relative MSE Improvement", 
               table_note="Improvement relative to Vague prior (negative = worse)")


# =============================================================================
# TABLE 10: POSTERIOR MEAN AND SD OF ALPHA
# Paper Table S3 (Online Supplement)
# =============================================================================

def table10_alpha_posterior():
    """Generate table with posterior mean and SD of hyperparameter alpha."""
    table = df.groupby(['K_true', 'I_target', 'method_label'])['post_alpha'].agg(['mean', 'std']).reset_index()
    table['Alpha'] = table.apply(lambda x: f"{x['mean']:.4f} ({x['std']:.4f})", axis=1)
    pivot = table.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Alpha')
    pivot = pivot[MAIN_METHODS]
    
    save_table(pivot, str(TABLES_SUPP / "table10_alpha_posterior.csv"), 
               table_title="Posterior Alpha", 
               table_note="Posterior mean and standard deviation of DP concentration parameter")


# =============================================================================
# TABLE 11: PRIOR HYPERPARAMETERS
# Paper Table S4 (Online Supplement)
# =============================================================================

def table11_prior_hyperparameters():
    """Generate table with prior hyperparameters (a, b), E[K], and P(w1>0.5)."""
    prior_table = pd.read_csv(str(DATA_DIR / "prior_table.csv"))
    
    save_table(prior_table, str(TABLES_SUPP / "table11_prior_hyperparameters.csv"), 
               table_title="Prior Hyperparameters", 
               table_note="Beta hyperparameters (a,b), prior mean E[K], and prior probability P(w1>0.5)")


# =============================================================================
# TABLE 12: EXACT AND NEAR MATCH PROBABILITIES
# Paper Table S5 (Online Supplement) and Table S5b (Online Supplement)
# =============================================================================

def table12_match_probabilities():
    """Generate tables with exact match P(K=K*) and near match P(|K-K*|<=1) probabilities."""
    # Exact match
    table_exact = df.groupby(['K_true', 'I_target', 'method_label'])['exact_match'].mean().reset_index()
    table_exact['Exact Match'] = table_exact['exact_match'].apply(lambda x: f"{x:.3f}")
    pivot_exact = table_exact.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Exact Match')
    pivot_exact = pivot_exact[MAIN_METHODS]
    
    save_table(pivot_exact, str(TABLES_SUPP / "table12_exact_match.csv"), 
               table_title="Exact Match Probability", 
               table_note="P(K=K*) = Probability that point estimate equals true value")
    
    # Near match
    table_near = df.groupby(['K_true', 'I_target', 'method_label'])['near_match'].mean().reset_index()
    table_near['Near Match'] = table_near['near_match'].apply(lambda x: f"{x:.3f}")
    pivot_near = table_near.pivot_table(index=['K_true', 'I_target'], columns='method_label', values='Near Match')
    pivot_near = pivot_near[MAIN_METHODS]
    
    save_table(pivot_near, str(TABLES_SUPP / "table12b_near_match.csv"), 
               table_title="Near Match Probability", 
               table_note="P(|K-K*|<=1) = Probability that point estimate is within 1 of true value")


# =============================================================================
# MAIN EXECUTION
# =============================================================================

if __name__ == "__main__":
    print("=" * 80)
    print("GENERATING TABLES FOR DPPRIOR SIMULATION STUDY")
    print("=" * 80)
    print()
    
    print("Generating MAIN TABLES (tables/main/):")
    print("-" * 80)
    table01_main_results()
    table02_collapse_probability()
    table03_bias_statistics()
    table04_mse_rmse()
    table05_coverage_ci()
    table06_low_info_summary()
    table09_relative_improvement()
    print()
    
    print("Generating SUPPLEMENT TABLES (tables/supplement/):")
    print("-" * 80)
    table07_all_methods()
    table08_confidence_comparison()
    table10_alpha_posterior()
    table11_prior_hyperparameters()
    table12_match_probabilities()
    print()
    
    print("=" * 80)
    print("ALL TABLES GENERATED SUCCESSFULLY")
    print("=" * 80)
