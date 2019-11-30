run:
	[ -f result.log ] && rm result.log || true
	stack runghc -- Main.hs 2>&1 | tee result.log

run-main2:
	[ -f result2.log ] && rm result2.log || true
	stack runghc -- Main2.hs 2>&1 | tee result2.log
