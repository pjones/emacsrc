DIRS = download packages emacs.d

all:
	@ for d in $(DIRS); do $(MAKE) -C $$d; done

clean:
	@ for d in $(DIRS); do $(MAKE) -C $$d clean; done
