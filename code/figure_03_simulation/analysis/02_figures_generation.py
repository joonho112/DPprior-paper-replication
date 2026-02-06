#!/usr/bin/env python3
"""
DPprior Replication Package
Step 2 of 3: Figures Generation

Part of: DPprior Simulation Study Analysis
Depends on:
  - 01_data_preparation.py (generates preprocessed_results.csv, main_results.csv)

Produces:
  - All figures in figures/main/ and figures/supplement/

Figure-to-Paper Mapping:
  Code Output File              → Paper Figure
  ─────────────────────────────────────────────
  fig01_simulation_grid          → Figure 3  (Main Text)
  fig03_collapse_probability     → Figure 4  (Main Text)
  fig04_bias_distribution        → Figure F1 (Appendix F)
  fig02_extended_grid            → Figure F2 (Appendix F): χ²-DORO comparison
  fig05_mse_comparison           → Figure F3 (Appendix F)
  fig10_heatmap_bias             → Figure F4 (Appendix F)
  fig06_coverage                 → Figure F5 (Appendix F)
  fig07_ci_width                 → Figure F6 (Appendix F)
  fig14_summary_low_info         → Figure F7 (Appendix F)
  fig08_confidence_sensitivity   → Figure F9 (Appendix F)
  fig11_alpha_posterior          → Figure F10 (Appendix F)
  fig09_posterior_sd             → Figure F11 (Appendix F)
  fig12_exact_match              → Figure F12 (Appendix F)
  fig15_mode_mean                → Figure F13 (Appendix F)

Author: JoonHo Lee
Date: January 2026
"""

import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D
import seaborn as sns
from scipy import stats
import warnings
warnings.filterwarnings('ignore')

# Set publication-quality defaults
plt.rcParams.update({
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'DejaVu Sans'],
    'font.size': 10,
    'axes.titlesize': 11,
    'axes.labelsize': 10,
    'xtick.labelsize': 9,
    'ytick.labelsize': 9,
    'legend.fontsize': 9,
    'figure.dpi': 300,
    'savefig.dpi': 300,
    'savefig.bbox': 'tight',
    'axes.spines.top': False,
    'axes.spines.right': False,
    'axes.linewidth': 0.8,
})

# Define repository root and paths
REPO_ROOT = Path(__file__).resolve().parents[3]
FIGURES_MAIN = REPO_ROOT / "figures" / "main"
FIGURES_SUPP = REPO_ROOT / "figures" / "supplement"

# Okabe-Ito colorblind-safe palette
OKABE_ITO = {
    'Vague': '#E69F00',        # Orange
    'DORO-Unif': '#56B4E9',    # Sky Blue
    'TSMM': '#009E73',         # Bluish Green
    'Dual-Anchor': '#CC79A7',  # Reddish Purple
    'χ²-DORO': '#0072B2',      # Blue
    'black': '#000000',
    'gray': '#999999'
}

# Method order
MAIN_METHODS = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor']
ALL_METHODS = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor', 'χ²-DORO']

# Load preprocessed data
df = pd.read_csv(REPO_ROOT / "data" / "simulation_results" / "preprocessed_results.csv")
df_main_medium = pd.read_csv(REPO_ROOT / "data" / "simulation_results" / "main_results.csv")


# =============================================================================
# FIGURE 1: Main 3x3 Grid - Posterior E[K_J] by Method (Main Text)
# =============================================================================
def figure1_main_grid(df, methods=MAIN_METHODS, confidence='medium', save_prefix='fig01'):
    # → Paper Figure 3 (Main Text): Posterior E[K_J] 3×3 grid
    """
    Main simulation results figure: 3x3 grid with K_true rows and I columns.
    Shows boxplots of posterior E[K_J | data] across methods.
    """
    # Filter data
    if confidence == 'medium':
        mask = df['confidence'].isin(['none', 'medium'])
    else:
        mask = df['confidence'].isin(['none', confidence])
    
    df_plot = df[mask & df['method_label'].isin(methods)].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(10, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            # Create boxplot
            positions = range(len(methods))
            bp_data = [subset[subset['method_label'] == m]['post_EK'].values for m in methods]
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.6, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            # Color boxes
            for patch, method in zip(bp['boxes'], methods):
                patch.set_facecolor(OKABE_ITO[method])
                patch.set_alpha(0.7)
            
            # Add horizontal reference line for true K
            ax.axhline(y=K, color='black', linestyle='--', linewidth=1, alpha=0.7, label='True K*')
            
            # Set x-axis labels
            ax.set_xticks(positions)
            ax.set_xticklabels([m.replace('-', '\n') for m in methods], fontsize=8)
            
            # Titles and labels
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold', fontsize=11)
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nPosterior E[K|data]', fontweight='bold')
            
            # Adjust y-axis based on K
            if K == 5:
                ax.set_ylim(0, max(15, subset['post_EK'].max() * 1.1))
            elif K == 10:
                ax.set_ylim(0, max(20, subset['post_EK'].max() * 1.1))
            else:
                ax.set_ylim(0, max(40, subset['post_EK'].max() * 1.1))
    
    # Add legend
    legend_elements = [mpatches.Patch(facecolor=OKABE_ITO[m], alpha=0.7, label=m) for m in methods]
    legend_elements.append(Line2D([0], [0], color='black', linestyle='--', label='True K*'))
    fig.legend(handles=legend_elements, loc='lower center', ncol=5, bbox_to_anchor=(0.5, -0.02))
    
    plt.suptitle('Posterior Expected Cluster Count by Method and Data Informativeness',
                 fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    # Save in multiple formats
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_simulation_grid.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_simulation_grid (pdf, png, jpg)")


# =============================================================================
# FIGURE 2: Extended with chi2-DORO (Supplement)
# =============================================================================
def figure2_extended_grid(df, save_prefix='fig02'):
    # → Paper Figure F2 (Appendix F): Extended grid with χ²-DORO
    """
    Extended figure including chi2-DORO for OSM.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(12, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            positions = range(len(ALL_METHODS))
            bp_data = [subset[subset['method_label'] == m]['post_EK'].values for m in ALL_METHODS]
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.6, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            for patch, method in zip(bp['boxes'], ALL_METHODS):
                patch.set_facecolor(OKABE_ITO[method])
                patch.set_alpha(0.7)
            
            ax.axhline(y=K, color='black', linestyle='--', linewidth=1, alpha=0.7)
            
            ax.set_xticks(positions)
            ax.set_xticklabels([m.replace('-', '\n').replace('χ²', 'χ²\n') for m in ALL_METHODS], fontsize=7)
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold', fontsize=11)
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nPosterior E[K|data]', fontweight='bold')
            
            if K == 5:
                ax.set_ylim(0, max(15, subset['post_EK'].max() * 1.1))
            elif K == 10:
                ax.set_ylim(0, max(20, subset['post_EK'].max() * 1.1))
            else:
                ax.set_ylim(0, max(40, subset['post_EK'].max() * 1.1))
    
    legend_elements = [mpatches.Patch(facecolor=OKABE_ITO[m], alpha=0.7, label=m) for m in ALL_METHODS]
    legend_elements.append(Line2D([0], [0], color='black', linestyle='--', label='True K*'))
    fig.legend(handles=legend_elements, loc='lower center', ncol=6, bbox_to_anchor=(0.5, -0.02))
    
    plt.suptitle('Posterior Expected Cluster Count (All Methods Including χ²-DORO)',
                 fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_extended_grid.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_extended_grid (pdf, png, jpg)")


# =============================================================================
# FIGURE 3: Collapse Probability P(K=1) 
# =============================================================================
def figure3_collapse_probability(df, save_prefix='fig03'):
    # → Paper Figure 4 (Main Text): Collapse probability P(K=1)
    """
    Figure showing P(K=1) - collapse to single cluster probability.
    Critical for understanding dominance behavior.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    # Aggregate
    collapse_summary = df_plot.groupby(['K_true', 'I_target', 'method_label'])['post_probK1'].agg(['mean', 'std']).reset_index()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4), sharey=True)
    
    bar_width = 0.2
    x = np.arange(len(K_values))
    
    for col, I in enumerate(I_values):
        ax = axes[col]
        
        for idx, method in enumerate(MAIN_METHODS):
            subset = collapse_summary[(collapse_summary['I_target'] == I) & 
                                      (collapse_summary['method_label'] == method)]
            means = [subset[subset['K_true'] == K]['mean'].values[0] for K in K_values]
            stds = [subset[subset['K_true'] == K]['std'].values[0] for K in K_values]
            
            ax.bar(x + idx * bar_width, means, bar_width, label=method,
                  color=OKABE_ITO[method], alpha=0.8, yerr=stds, capsize=2)
        
        ax.set_xlabel('True K*')
        ax.set_xticks(x + bar_width * 1.5)
        ax.set_xticklabels(K_values)
        ax.set_title(f'I = {I}', fontweight='bold')
        
        if col == 0:
            ax.set_ylabel('P(K = 1)', fontweight='bold')
    
    axes[0].legend(loc='upper right', fontsize=8)
    
    plt.suptitle('Probability of Cluster Collapse (K = 1) Across Conditions',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_collapse_probability.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_collapse_probability (pdf, png, jpg)")


# =============================================================================
# FIGURE 4: Bias Distribution
# =============================================================================
def figure4_bias_distribution(df, save_prefix='fig04'):
    # → Paper Figure F1 (Appendix F): Bias distribution
    """
    Violin plots showing distribution of bias (E[K] - K*).
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(11, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            # Create violin plot data
            plot_data = []
            for method in MAIN_METHODS:
                method_data = subset[subset['method_label'] == method]['bias_K'].values
                plot_data.append(method_data)
            
            parts = ax.violinplot(plot_data, positions=range(len(MAIN_METHODS)),
                                  showmeans=True, showmedians=False)
            
            for idx, (pc, method) in enumerate(zip(parts['bodies'], MAIN_METHODS)):
                pc.set_facecolor(OKABE_ITO[method])
                pc.set_alpha(0.7)
            
            parts['cmeans'].set_color('black')
            parts['cmeans'].set_linewidth(1.5)
            
            ax.axhline(y=0, color='black', linestyle='--', linewidth=1, alpha=0.7)
            
            ax.set_xticks(range(len(MAIN_METHODS)))
            ax.set_xticklabels([m.replace('-', '\n') for m in MAIN_METHODS], fontsize=8)
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold')
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nBias (E[K] - K*)', fontweight='bold')
    
    plt.suptitle('Distribution of Bias in Posterior Expected Cluster Count',
                fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_bias_distribution.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_bias_distribution (pdf, png, jpg)")


# =============================================================================
# FIGURE 5: MSE Comparison
# =============================================================================
def figure5_mse_comparison(df, save_prefix='fig05'):
    # → Paper Figure F3 (Appendix F): MSE comparison
    """
    Bar chart comparing MSE across methods.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    mse_summary = df_plot.groupby(['K_true', 'I_target', 'method_label'])['mse_K'].agg(['mean', 'std']).reset_index()
    mse_summary.columns = ['K_true', 'I_target', 'method_label', 'mse_mean', 'mse_std']
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    
    bar_width = 0.2
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    x = np.arange(len(K_values))
    
    for col, I in enumerate(I_values):
        ax = axes[col]
        
        for idx, method in enumerate(MAIN_METHODS):
            subset = mse_summary[(mse_summary['I_target'] == I) & 
                                 (mse_summary['method_label'] == method)]
            means = [subset[subset['K_true'] == K]['mse_mean'].values[0] for K in K_values]
            stds = [subset[subset['K_true'] == K]['mse_std'].values[0] for K in K_values]
            
            ax.bar(x + idx * bar_width, means, bar_width, label=method,
                  color=OKABE_ITO[method], alpha=0.8)
        
        ax.set_xlabel('True K*')
        ax.set_xticks(x + bar_width * 1.5)
        ax.set_xticklabels(K_values)
        ax.set_title(f'I = {I}', fontweight='bold')
        ax.set_yscale('log')  # Log scale for MSE
        
        if col == 0:
            ax.set_ylabel('MSE (log scale)', fontweight='bold')
    
    axes[2].legend(loc='upper left', fontsize=8)
    
    plt.suptitle('Mean Squared Error of Posterior E[K] Relative to True K*',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_mse_comparison.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_mse_comparison (pdf, png, jpg)")


# =============================================================================
# FIGURE 6: Coverage Probability
# =============================================================================
def figure6_coverage(df, save_prefix='fig06'):
    # → Paper Figure F5 (Appendix F): Coverage probability
    """
    Bar chart showing 95% CI coverage of true K*.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    coverage_summary = df_plot.groupby(['K_true', 'I_target', 'method_label'])['covers_true'].mean().reset_index()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    
    bar_width = 0.2
    x = np.arange(len(K_values))
    
    for col, I in enumerate(I_values):
        ax = axes[col]
        
        for idx, method in enumerate(MAIN_METHODS):
            subset = coverage_summary[(coverage_summary['I_target'] == I) & 
                                      (coverage_summary['method_label'] == method)]
            means = [subset[subset['K_true'] == K]['covers_true'].values[0] for K in K_values]
            
            ax.bar(x + idx * bar_width, means, bar_width, label=method,
                  color=OKABE_ITO[method], alpha=0.8)
        
        ax.axhline(y=0.95, color='black', linestyle='--', linewidth=1, alpha=0.7, label='Nominal 95%')
        
        ax.set_xlabel('True K*')
        ax.set_xticks(x + bar_width * 1.5)
        ax.set_xticklabels(K_values)
        ax.set_title(f'I = {I}', fontweight='bold')
        ax.set_ylim(0, 1.05)
        
        if col == 0:
            ax.set_ylabel('Coverage Probability', fontweight='bold')
    
    axes[2].legend(loc='lower right', fontsize=8)
    
    plt.suptitle('95% Credible Interval Coverage of True K*',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_coverage.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_coverage (pdf, png, jpg)")


# =============================================================================
# FIGURE 7: Credible Interval Width
# =============================================================================
def figure7_ci_width(df, save_prefix='fig07'):
    # → Paper Figure F6 (Appendix F): Confidence interval width
    """
    Box plots of 95% CI width by method.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(10, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            positions = range(len(MAIN_METHODS))
            bp_data = [subset[subset['method_label'] == m]['ci_width'].values for m in MAIN_METHODS]
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.6, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            for patch, method in zip(bp['boxes'], MAIN_METHODS):
                patch.set_facecolor(OKABE_ITO[method])
                patch.set_alpha(0.7)
            
            ax.set_xticks(positions)
            ax.set_xticklabels([m.replace('-', '\n') for m in MAIN_METHODS], fontsize=8)
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold')
            if j == 0:
                ax.set_ylabel(f'K* = {K}\n95% CI Width', fontweight='bold')
    
    plt.suptitle('Width of 95% Credible Interval for K',
                fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_ci_width.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_ci_width (pdf, png, jpg)")


# =============================================================================
# FIGURE 8: Confidence Level Sensitivity (TSMM Only)
# =============================================================================
def figure8_confidence_sensitivity(df, save_prefix='fig08'):
    # → Paper Figure F9 (Appendix F): Confidence sensitivity
    """
    Shows how confidence level affects TSMM and Dual-Anchor performance.
    """
    tsmm_dual = df[df['method'].isin(['tsmm', 'dual'])].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    conf_levels = ['low', 'medium', 'high']
    
    fig, axes = plt.subplots(3, 3, figsize=(11, 9))
    
    colors_conf = {'low': '#D55E00', 'medium': '#009E73', 'high': '#0072B2'}
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = tsmm_dual[(tsmm_dual['K_true'] == K) & (tsmm_dual['I_target'] == I)]
            
            positions = []
            bp_data = []
            colors = []
            
            offset = 0
            for method in ['tsmm', 'dual']:
                for conf in conf_levels:
                    data = subset[(subset['method'] == method) & (subset['confidence'] == conf)]['post_EK'].values
                    if len(data) > 0:
                        bp_data.append(data)
                        positions.append(offset)
                        colors.append(colors_conf[conf])
                    offset += 0.5
                offset += 0.5  # Extra space between methods
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.4, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            for patch, color in zip(bp['boxes'], colors):
                patch.set_facecolor(color)
                patch.set_alpha(0.7)
            
            ax.axhline(y=K, color='black', linestyle='--', linewidth=1, alpha=0.7)
            
            # Set x-ticks
            ax.set_xticks([0.5, 3.5])
            ax.set_xticklabels(['TSMM', 'Dual-Anchor'])
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold')
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nPosterior E[K|data]', fontweight='bold')
    
    # Legend for confidence levels
    legend_elements = [mpatches.Patch(facecolor=colors_conf[c], alpha=0.7, label=c.capitalize()) 
                      for c in conf_levels]
    legend_elements.append(Line2D([0], [0], color='black', linestyle='--', label='True K*'))
    fig.legend(handles=legend_elements, loc='lower center', ncol=4, bbox_to_anchor=(0.5, -0.02))
    
    plt.suptitle('Effect of Confidence Level on TSMM and Dual-Anchor Methods',
                fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_confidence_sensitivity.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_confidence_sensitivity (pdf, png, jpg)")


# =============================================================================
# FIGURE 9: Posterior SD Comparison
# =============================================================================
def figure9_posterior_sd(df, save_prefix='fig09'):
    # → Paper Figure F11 (Appendix F): Posterior SD of K
    """
    Shows posterior standard deviation of K across methods.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(10, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            positions = range(len(MAIN_METHODS))
            bp_data = [subset[subset['method_label'] == m]['post_sdK'].values for m in MAIN_METHODS]
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.6, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            for patch, method in zip(bp['boxes'], MAIN_METHODS):
                patch.set_facecolor(OKABE_ITO[method])
                patch.set_alpha(0.7)
            
            ax.set_xticks(positions)
            ax.set_xticklabels([m.replace('-', '\n') for m in MAIN_METHODS], fontsize=8)
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold')
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nPosterior SD(K)', fontweight='bold')
    
    plt.suptitle('Posterior Standard Deviation of Cluster Count',
                fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_posterior_sd.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_posterior_sd (pdf, png, jpg)")


# =============================================================================
# FIGURE 10: Relative Bias Heatmap
# =============================================================================
def figure10_heatmap(df, save_prefix='fig10'):
    # → Paper Figure F4 (Appendix F): Relative bias heatmap
    """
    Heatmap of relative bias (%) across conditions.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    rel_bias = df_plot.groupby(['K_true', 'I_target', 'method_label'])['rel_bias_K'].mean().reset_index()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(1, 4, figsize=(14, 4))
    
    for idx, method in enumerate(MAIN_METHODS):
        ax = axes[idx]
        
        subset = rel_bias[rel_bias['method_label'] == method]
        pivot_data = subset.pivot(index='K_true', columns='I_target', values='rel_bias_K')
        pivot_data = pivot_data.reindex(K_values[::-1])  # Reverse order for display
        
        im = ax.imshow(pivot_data.values, cmap='RdYlGn_r', aspect='auto',
                       vmin=-80, vmax=80)
        
        # Add text annotations
        for i in range(len(K_values)):
            for j in range(len(I_values)):
                val = pivot_data.iloc[i, j]
                color = 'white' if abs(val) > 40 else 'black'
                ax.text(j, i, f'{val:.1f}%', ha='center', va='center', color=color, fontsize=9)
        
        ax.set_xticks(range(len(I_values)))
        ax.set_xticklabels(I_values)
        ax.set_xlabel('Informativeness (I)')
        
        ax.set_yticks(range(len(K_values)))
        ax.set_yticklabels(K_values[::-1])
        
        if idx == 0:
            ax.set_ylabel('True K*')
        
        ax.set_title(method, fontweight='bold')
    
    cbar = fig.colorbar(im, ax=axes.ravel().tolist(), shrink=0.6, pad=0.02)
    cbar.set_label('Relative Bias (%)')
    
    plt.suptitle('Relative Bias in Posterior E[K] by Method',
                fontweight='bold', fontsize=12)
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_heatmap_bias.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_heatmap_bias (pdf, png, jpg)")


# =============================================================================
# FIGURE 11: Alpha Posterior Mean
# =============================================================================
def figure11_alpha_posterior(df, save_prefix='fig11'):
    # → Paper Figure F10 (Appendix F): Posterior E[alpha]
    """
    Shows posterior mean of alpha across methods.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(3, 3, figsize=(10, 9))
    
    for i, K in enumerate(K_values):
        for j, I in enumerate(I_values):
            ax = axes[i, j]
            
            subset = df_plot[(df_plot['K_true'] == K) & (df_plot['I_target'] == I)]
            
            positions = range(len(MAIN_METHODS))
            bp_data = [subset[subset['method_label'] == m]['alpha_mean'].values for m in MAIN_METHODS]
            
            bp = ax.boxplot(bp_data, positions=positions, widths=0.6, patch_artist=True,
                           showfliers=False, medianprops=dict(color='black', linewidth=1.5))
            
            for patch, method in zip(bp['boxes'], MAIN_METHODS):
                patch.set_facecolor(OKABE_ITO[method])
                patch.set_alpha(0.7)
            
            ax.set_xticks(positions)
            ax.set_xticklabels([m.replace('-', '\n') for m in MAIN_METHODS], fontsize=8)
            
            if i == 0:
                ax.set_title(f'I = {I}', fontweight='bold')
            if j == 0:
                ax.set_ylabel(f'K* = {K}\nPosterior E[α|data]', fontweight='bold')
    
    plt.suptitle('Posterior Mean of Concentration Parameter α',
                fontweight='bold', fontsize=12, y=1.01)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_alpha_posterior.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_alpha_posterior (pdf, png, jpg)")


# =============================================================================
# FIGURE 12: P(K = K*) - Exact Match Probability
# =============================================================================
def figure12_exact_match(df, save_prefix='fig12'):
    # → Paper Figure F12 (Appendix F): Exact match P(K=K*)
    """
    Bar chart showing P(K = K_true) - probability of exact match.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    exact_match = df_plot.groupby(['K_true', 'I_target', 'method_label'])['post_probK_eq_true'].mean().reset_index()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    
    bar_width = 0.2
    x = np.arange(len(K_values))
    
    for col, I in enumerate(I_values):
        ax = axes[col]
        
        for idx, method in enumerate(MAIN_METHODS):
            subset = exact_match[(exact_match['I_target'] == I) & 
                                 (exact_match['method_label'] == method)]
            means = [subset[subset['K_true'] == K]['post_probK_eq_true'].values[0] for K in K_values]
            
            ax.bar(x + idx * bar_width, means, bar_width, label=method,
                  color=OKABE_ITO[method], alpha=0.8)
        
        ax.set_xlabel('True K*')
        ax.set_xticks(x + bar_width * 1.5)
        ax.set_xticklabels(K_values)
        ax.set_title(f'I = {I}', fontweight='bold')
        ax.set_ylim(0, 0.35)
        
        if col == 0:
            ax.set_ylabel('P(K = K*)', fontweight='bold')
    
    axes[2].legend(loc='upper right', fontsize=8)
    
    plt.suptitle('Probability of Exact Match: P(K = K*)',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_exact_match.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_exact_match (pdf, png, jpg)")


# =============================================================================
# FIGURE 13: P(K within 1) - Near Match Probability
# =============================================================================
def figure13_near_match(df, save_prefix='fig13'):
    # → Paper Figure F13 (Appendix F): Near match P(|K-K*|≤1)
    """
    Bar chart showing P(|K - K_true| <= 1) - probability of near match.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    near_match = df_plot.groupby(['K_true', 'I_target', 'method_label'])['post_probK_within1'].mean().reset_index()
    
    K_values = [5, 10, 30]
    I_values = [0.2, 0.5, 0.8]
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    
    bar_width = 0.2
    x = np.arange(len(K_values))
    
    for col, I in enumerate(I_values):
        ax = axes[col]
        
        for idx, method in enumerate(MAIN_METHODS):
            subset = near_match[(near_match['I_target'] == I) & 
                                (near_match['method_label'] == method)]
            means = [subset[subset['K_true'] == K]['post_probK_within1'].values[0] for K in K_values]
            
            ax.bar(x + idx * bar_width, means, bar_width, label=method,
                  color=OKABE_ITO[method], alpha=0.8)
        
        ax.set_xlabel('True K*')
        ax.set_xticks(x + bar_width * 1.5)
        ax.set_xticklabels(K_values)
        ax.set_title(f'I = {I}', fontweight='bold')
        ax.set_ylim(0, 0.6)
        
        if col == 0:
            ax.set_ylabel('P(|K - K*| ≤ 1)', fontweight='bold')
    
    axes[2].legend(loc='upper right', fontsize=8)
    
    plt.suptitle('Probability of Near Match: P(|K - K*| ≤ 1)',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_near_match.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_near_match (pdf, png, jpg)")


# =============================================================================
# FIGURE 14: Summary Panel (Combined Key Metrics)
# =============================================================================
def figure14_summary_panel(df, save_prefix='fig14'):
    # → Paper Figure F7 (Appendix F): Summary panel (low info)
    """
    Summary panel combining key metrics in one figure.
    Focus on I=0.2 (low informativeness) where methods differ most.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS) & (df['I_target'] == 0.2)].copy()
    
    K_values = [5, 10, 30]
    
    fig, axes = plt.subplots(2, 2, figsize=(10, 8))
    
    bar_width = 0.2
    x = np.arange(len(K_values))
    
    # Panel A: Mean E[K]
    ax = axes[0, 0]
    mean_K = df_plot.groupby(['K_true', 'method_label'])['post_EK'].mean().reset_index()
    for idx, method in enumerate(MAIN_METHODS):
        subset = mean_K[mean_K['method_label'] == method]
        means = [subset[subset['K_true'] == K]['post_EK'].values[0] for K in K_values]
        ax.bar(x + idx * bar_width, means, bar_width, label=method,
              color=OKABE_ITO[method], alpha=0.8)
    # Reference line for true K
    for kidx, K in enumerate(K_values):
        ax.hlines(y=K, xmin=kidx - 0.2, xmax=kidx + 0.7, colors='black', 
                  linestyles='--', linewidth=1, alpha=0.5)
    ax.set_xticks(x + bar_width * 1.5)
    ax.set_xticklabels(K_values)
    ax.set_xlabel('True K*')
    ax.set_ylabel('Mean Posterior E[K]')
    ax.set_title('(A) Mean Posterior E[K|data]', fontweight='bold')
    ax.legend(loc='upper left', fontsize=8)
    
    # Panel B: MSE
    ax = axes[0, 1]
    mse = df_plot.groupby(['K_true', 'method_label'])['mse_K'].mean().reset_index()
    for idx, method in enumerate(MAIN_METHODS):
        subset = mse[mse['method_label'] == method]
        means = [subset[subset['K_true'] == K]['mse_K'].values[0] for K in K_values]
        ax.bar(x + idx * bar_width, means, bar_width, label=method,
              color=OKABE_ITO[method], alpha=0.8)
    ax.set_xticks(x + bar_width * 1.5)
    ax.set_xticklabels(K_values)
    ax.set_xlabel('True K*')
    ax.set_ylabel('MSE')
    ax.set_title('(B) Mean Squared Error', fontweight='bold')
    ax.set_yscale('log')
    
    # Panel C: Collapse P(K=1)
    ax = axes[1, 0]
    collapse = df_plot.groupby(['K_true', 'method_label'])['post_probK1'].mean().reset_index()
    for idx, method in enumerate(MAIN_METHODS):
        subset = collapse[collapse['method_label'] == method]
        means = [subset[subset['K_true'] == K]['post_probK1'].values[0] for K in K_values]
        ax.bar(x + idx * bar_width, means, bar_width, label=method,
              color=OKABE_ITO[method], alpha=0.8)
    ax.set_xticks(x + bar_width * 1.5)
    ax.set_xticklabels(K_values)
    ax.set_xlabel('True K*')
    ax.set_ylabel('P(K = 1)')
    ax.set_title('(C) Cluster Collapse Probability', fontweight='bold')
    
    # Panel D: Coverage
    ax = axes[1, 1]
    cov = df_plot.groupby(['K_true', 'method_label'])['covers_true'].mean().reset_index()
    for idx, method in enumerate(MAIN_METHODS):
        subset = cov[cov['method_label'] == method]
        means = [subset[subset['K_true'] == K]['covers_true'].values[0] for K in K_values]
        ax.bar(x + idx * bar_width, means, bar_width, label=method,
              color=OKABE_ITO[method], alpha=0.8)
    ax.axhline(y=0.95, color='black', linestyle='--', linewidth=1, alpha=0.7)
    ax.set_xticks(x + bar_width * 1.5)
    ax.set_xticklabels(K_values)
    ax.set_xlabel('True K*')
    ax.set_ylabel('Coverage')
    ax.set_title('(D) 95% CI Coverage', fontweight='bold')
    ax.set_ylim(0, 1.05)
    
    plt.suptitle('Summary of Method Performance in Low-Information Setting (I = 0.2)',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_MAIN / f"{save_prefix}_summary_low_info.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_summary_low_info (pdf, png, jpg)")


# =============================================================================
# FIGURE 15: Mode vs Mean Comparison
# =============================================================================
def figure15_mode_mean(df, save_prefix='fig15'):
    # → Paper Figure F13 (Appendix F): Mode vs mean comparison
    """
    Scatter plot comparing posterior mode vs mean of K.
    """
    mask = df['confidence'].isin(['none', 'medium'])
    df_plot = df[mask & df['method_label'].isin(MAIN_METHODS)].copy()
    
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    
    for col, I in enumerate([0.2, 0.5, 0.8]):
        ax = axes[col]
        
        subset = df_plot[df_plot['I_target'] == I]
        
        for method in MAIN_METHODS:
            method_data = subset[subset['method_label'] == method]
            ax.scatter(method_data['post_modeK'], method_data['post_EK'],
                      alpha=0.3, label=method, color=OKABE_ITO[method], s=10)
        
        # Add identity line
        max_val = max(subset['post_EK'].max(), subset['post_modeK'].max())
        ax.plot([0, max_val], [0, max_val], 'k--', alpha=0.5, label='Identity')
        
        ax.set_xlabel('Posterior Mode(K)')
        ax.set_ylabel('Posterior E[K]')
        ax.set_title(f'I = {I}', fontweight='bold')
        
        if col == 2:
            ax.legend(loc='lower right', fontsize=8)
    
    plt.suptitle('Comparison of Posterior Mode vs Mean of Cluster Count',
                fontweight='bold', fontsize=12)
    
    plt.tight_layout()
    
    for fmt in ['pdf', 'png', 'jpg']:
        fig.savefig(str(FIGURES_SUPP / f"{save_prefix}_mode_mean.{fmt}"),
                   dpi=300, bbox_inches='tight')
    
    plt.close()
    print(f"✓ Saved: {save_prefix}_mode_mean (pdf, png, jpg)")


# =============================================================================
# RUN ALL FIGURES
# =============================================================================
if __name__ == "__main__":
    print("\n" + "=" * 60)
    print("GENERATING FIGURES")
    print("=" * 60 + "\n")
    
    # Main text figures
    print("--- Main Text Figures ---")
    figure1_main_grid(df, methods=MAIN_METHODS, confidence='medium', save_prefix='fig01')
    figure3_collapse_probability(df, save_prefix='fig03')
    figure4_bias_distribution(df, save_prefix='fig04')
    figure5_mse_comparison(df, save_prefix='fig05')
    figure6_coverage(df, save_prefix='fig06')
    figure7_ci_width(df, save_prefix='fig07')
    figure10_heatmap(df, save_prefix='fig10')
    figure14_summary_panel(df, save_prefix='fig14')
    
    # Supplement figures
    print("\n--- Supplement Figures ---")
    figure2_extended_grid(df, save_prefix='fig02')
    figure8_confidence_sensitivity(df, save_prefix='fig08')
    figure9_posterior_sd(df, save_prefix='fig09')
    figure11_alpha_posterior(df, save_prefix='fig11')
    figure12_exact_match(df, save_prefix='fig12')
    figure13_near_match(df, save_prefix='fig13')
    figure15_mode_mean(df, save_prefix='fig15')
    
    print("\n" + "=" * 60)
    print("ALL FIGURES GENERATED SUCCESSFULLY!")
    print("=" * 60)
