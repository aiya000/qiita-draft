#!/bin/bash
ls ~/.backup/neovim-backup/ | rg -v 'archive|swp|undo' | while read dir ; do
  backuped_dir=~/.backup/neovim-backup/$dir
  echo
  tree "$backuped_dir" | ./tree-limit.awk | sed 's;%;/;g'
done
