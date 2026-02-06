# Makefile (optional) — v2
# Author: JoonHo Lee

CONFIG=config/fig3_design.yaml

step0:
	Rscript scripts/step0_check_env.R --config $(CONFIG)

step1:
	Rscript scripts/step1_calibrate_I.R --config $(CONFIG)

step1_test:
	Rscript scripts/step1_test_calibration.R --config $(CONFIG)

step2:
	Rscript scripts/step2_build_priors.R --config $(CONFIG)

step2_test:
	Rscript scripts/step2_test_priors.R --config $(CONFIG)

smoke:
	Rscript scripts/step3_smoketest.R --config $(CONFIG)

pilot:
	Rscript scripts/step3_run_pilot.R --config $(CONFIG)

full_1_50:
	Rscript scripts/step3_run_full.R --config $(CONFIG) --rep_start 1 --rep_end 50

merge:
	Rscript scripts/step3_merge_results.R --config $(CONFIG)

plot:
	Rscript scripts/step4_plot_fig3.R --config $(CONFIG)

plot_all:
	Rscript scripts/step4_plot_fig3_all.R --config $(CONFIG)
