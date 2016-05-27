# ===== Run test and visualize results =====

library(vtest)

# Run the tests on ggpmosaic
vtest('.')

# Generate webpages showing results
vtest_webpage()


# ===== Compare to previous results =====

# Find recent commits with saved resultsets
recent_vtest()

# Compare it to the most recent commit with saved results.
# This will print out the items that have changed
vdiffstat("e1dec3f", "")

# Generate web page showing differences
vdiff_webpage("e1dec3f", "")


# ===== Save results =====
# We didn't save the resultset from the last test when we ran it
# but we can save it now:
save_last_resultset()

# see the wiki pages and help pages for the vtest functions for more info:

#  https://github.com/wch/ggplot2/wiki/Visual-test-system-instructions
#  https://github.com/wch/ggplot2/wiki/Visual-test-system

