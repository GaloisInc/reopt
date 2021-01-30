default: nweb23_static_freebsd.ann

.PHONY : default clean

clean :
	rm -f nweb23_static_freebsd.ll nweb23_static_freebsd.ann


# We use '-nostdlib' to get a minimal binary.
division_by_zero: division_by_zero.c
	gcc -nostdlib $< -o $@

nweb23_static_freebsd.ll nweb23_static_freebsd.ann : nweb23_static_freebsd Makefile
	reopt -o nweb23_static_freebsd.ll \
              --llvm \
              --annotations nweb23_static_freebsd.ann \
              --exclude extent_tree_ad_search \
              --exclude grouping_print \
              --exclude __build_env \
              --exclude __sfvwrite \
              --exclude catgets \
              --exclude settzname \
              --exclude __wrap_setrunelocale \
              --exclude _Read_RuneMagi \
              --exclude _collate_load_tables \
              --exclude __part_load_locale \
              --exclude bcopy \
              --exclude memmove \
              --exclude memcpy \
              --exclude start \
              --exclude init_aux \
              $<

nweb23_static_freebsd.bc : nweb23_static_freebsd.ll
	llvm-as -o=$@ $<
