#!/bin/sh

indent=0
src=""
nl='
'
IFS=''

codl() {
  case $1 in
    begin)
      for ((i = 0; i < $indent; i++))
      do src="$src  "
      done
      indent=$(($indent + 1))
      src="$src$2$nl" ;;
    put)
      for ((i = 0; i < $indent; i++))
      do src="$src  "
      done
      src="$src$2 $3$nl" ;;
    putl)
      for ((i = 0; i < $indent; i++))
      do src="$src  "
      done
      src="$src$2$nl"
      while read -r line
      do
        for ((i = 0; i < $indent; i++))
        do src="$src      $line$nl"
        done
      done < <(echo "$3") ;;
    end)
      indent=$(($indent - 1)) ;;
    term)
      src="$src##$nl" ;;
    *)
      return 1
  esac
}

