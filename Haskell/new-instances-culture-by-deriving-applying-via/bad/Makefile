run:
	[ -f result.log ] && rm result.log || true
	stack runghc -- Main.hs 2>&1 | tee result.log
