#!/usr/bin/env python3
"""
DPprior Replication Package - Simulation Study Analysis
Pipeline Step 1 of 3: Data Preparation

This script loads raw simulation results and preprocesses them for analysis.
It creates standardized labels, computes derived metrics, and generates
analysis-ready datasets for both main text and supplementary analyses.

Input Files:
  - data/simulation_results/raw_results_merged.csv
  - data/simulation_results/prior_table.csv
  - data/simulation_results/informativeness_calibration.csv

Output Files:
  - data/simulation_results/preprocessed_results.csv (all methods, all confidence levels)
  - data/simulation_results/main_results.csv (main methods, filtered confidence levels)
  - figures/main/ (directory for main text figures)
  - figures/supplement/ (directory for supplementary figures)
  - tables/main/ (directory for main text tables)
  - tables/supplement/ (directory for supplementary tables)

Usage:
  python 01_data_preparation.py

Author: JoonHo Lee
Date: January 2026
"""

import pandas as pd
import numpy as np
from pathlib import Path


def get_repo_root():
    """Calculate repository root from script location."""
    return Path(__file__).resolve().parents[3]


def create_output_directories(repo_root):
    """Create necessary output directories."""
    output_dirs = [
        repo_root / "figures" / "main",
        repo_root / "figures" / "supplement",
        repo_root / "tables" / "main",
        repo_root / "tables" / "supplement",
    ]
    for output_dir in output_dirs:
        output_dir.mkdir(parents=True, exist_ok=True)


def load_input_data(repo_root):
    """Load raw data files from data directory."""
    data_dir = repo_root / "data" / "simulation_results"
    
    raw_results = pd.read_csv(data_dir / "raw_results_merged.csv")
    prior_table = pd.read_csv(data_dir / "prior_table.csv")
    info_calibration = pd.read_csv(data_dir / "informativeness_calibration.csv")
    
    return raw_results, prior_table, info_calibration


def print_data_summary(raw_results):
    """Print summary statistics of loaded data."""
    print("=" * 60)
    print("DATA SUMMARY")
    print("=" * 60)
    print(f"\nRaw Results Shape: {raw_results.shape}")
    print(f"Prior Table Shape: {raw_results.shape}")
    print(f"Informativeness Calibration Shape: {raw_results.shape}")

    # Check unique values
    print("\n--- Unique Values ---")
    print(f"K_true: {sorted(raw_results['K_true'].unique())}")
    print(f"I_target: {sorted(raw_results['I_target'].unique())}")
    print(f"Methods: {raw_results['method'].unique()}")
    print(f"Confidence: {raw_results['confidence'].unique()}")

    # Count replications
    print("\n--- Replication Counts ---")
    rep_counts = raw_results.groupby(['K_true', 'I_target', 'method', 'confidence']).size().reset_index(name='n_reps')
    print(rep_counts.groupby(['K_true', 'I_target']).size().head(20))

    # Check for missing/failed runs
    status_counts = raw_results['status'].value_counts()
    print(f"\n--- Status Counts ---")
    print(status_counts)


def preprocess_data(df):
    """
    Clean and prepare data for analysis.
    
    Creates standardized labels for methods, informativeness levels, and confidence,
    computes derived metrics (bias, MSE, coverage), and applies categorical ordering
    for visualization consistency.
    """
    df = df.copy()
    
    # Filter to successful runs only
    df = df[df['status'] == 'ok']
    
    # Create method labels for publication
    method_labels = {
        'vague': 'Vague',
        'doro_uniform': 'DORO-Unif',
        'tsmm': 'TSMM',
        'dual': 'Dual-Anchor',
        'chi2_doro': 'χ²-DORO'
    }
    df['method_label'] = df['method'].map(method_labels)
    
    # Create ordered categorical for methods
    main_order = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor']
    all_order = ['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor', 'χ²-DORO']
    
    df['method_ordered'] = pd.Categorical(df['method_label'], categories=all_order, ordered=True)
    
    # Create informativeness labels
    df['I_label'] = df['I_target'].map({0.2: 'Low (I=0.2)', 0.5: 'Medium (I=0.5)', 0.8: 'High (I=0.8)'})
    df['I_ordered'] = pd.Categorical(df['I_label'], 
                                      categories=['Low (I=0.2)', 'Medium (I=0.5)', 'High (I=0.8)'], 
                                      ordered=True)
    
    # Create K labels
    df['K_label'] = df['K_true'].map({5: 'K* = 5', 10: 'K* = 10', 30: 'K* = 30'})
    df['K_ordered'] = pd.Categorical(df['K_label'], 
                                      categories=['K* = 5', 'K* = 10', 'K* = 30'], 
                                      ordered=True)
    
    # Confidence labels
    conf_labels = {
        'none': 'None',
        'low': 'Low',
        'medium': 'Medium', 
        'high': 'High'
    }
    df['conf_label'] = df['confidence'].map(conf_labels)
    df['conf_ordered'] = pd.Categorical(df['conf_label'],
                                         categories=['None', 'Low', 'Medium', 'High'],
                                         ordered=True)
    
    # Compute additional metrics
    df['bias_K'] = df['post_EK'] - df['K_true']
    df['abs_bias_K'] = np.abs(df['bias_K'])
    df['rel_bias_K'] = (df['post_EK'] - df['K_true']) / df['K_true'] * 100
    df['mse_K'] = df['bias_K'] ** 2
    df['collapse_prob'] = df['post_probK1']  # P(K=1)
    df['ci_width'] = df['post_q975'] - df['post_q025']
    
    # Coverage indicator (true K within 95% CI)
    df['covers_true'] = ((df['post_q025'] <= df['K_true']) & (df['K_true'] <= df['post_q975'])).astype(int)
    
    return df


def save_preprocessed_data(df, df_main_medium, repo_root):
    """Save preprocessed datasets to CSV files."""
    data_dir = repo_root / "data" / "simulation_results"
    
    df.to_csv(data_dir / "preprocessed_results.csv", index=False)
    df_main_medium.to_csv(data_dir / "main_results.csv", index=False)


if __name__ == "__main__":
    # Initialize repository paths
    repo_root = get_repo_root()
    
    # Create output directories
    create_output_directories(repo_root)
    
    # Load data
    raw_results, prior_table, info_calibration = load_input_data(repo_root)
    
    # Print data summary
    print_data_summary(raw_results)
    
    # Preprocess data
    df = preprocess_data(raw_results)
    print("\n--- After Preprocessing ---")
    print(f"Shape: {df.shape}")
    
    # Create subsets for main text (excluding chi2_doro and filtering to medium confidence or none)
    df_main = df[df['method_label'].isin(['Vague', 'DORO-Unif', 'TSMM', 'Dual-Anchor'])]
    df_main_medium = df_main[df_main['confidence'].isin(['none', 'medium'])]
    print(f"Main text subset (medium conf): {df_main_medium.shape}")
    
    # All methods including chi2_doro for supplement
    df_all = df.copy()
    print(f"All methods (supplement): {df_all.shape}")
    
    # Save preprocessed data
    save_preprocessed_data(df, df_main_medium, repo_root)
    
    print("\n✓ Data preprocessing complete!")
    print(f"  Saved: preprocessed_results.csv")
    print(f"  Saved: main_results.csv")
