
function classpath() {
  local MOD="$1"
  local JAR="$(echo $MOD | tr '/' '-').jar"
  local PATTERN="$(echo $MOD | tr '/' '.'):"
  local REFS="$(cat refs | grep "$PATTERN" | cut -d: -f2)"
    local BINS="$(cat bins | grep "$PATTERN" | sed 's/^[^:]*: //')"
  
  for REF in $REFS; do
    classpath $REF
  done | sort | uniq
  
  for BIN in $BINS; do
    cs fetch $BIN
  done | sort | uniq

  echo "pub/$JAR"
}

function build() {
  local MOD="$1"
  local JAR="$(echo $MOD | tr '/' '-').jar"
  local PROJECT="$(echo $MOD | cut -d/ -f1)"
  local ID="$(echo $MOD | cut -d/ -f2)"
  local SRC="mod/$PROJECT/src/$ID"
  local PATTERN="$(echo $MOD | tr '/' '.'):"
  local REFS="$(cat refs | grep "$PATTERN" | cut -d: -f2)"
  
  if [ ! -f "pub/$JAR" ]; then
    for REF in $REFS; do
      build "$REF"
    done
  
    local CP="$(classpath "$MOD" | grep -v "$PATTERN" | tr '\n' ':')"
    
    if [ -z "$CP" ]; then
      local CPF=""
    else
      local CPF="-classpath:"
    fi
   
    echo "Building $MOD..."
    #echo scala/bin/scalac ${CPF}${CP%?} -d out -language:experimental.saferExceptions $SRC/*.scala
    scala/bin/scalac ${CPF}${CP%?} -d out -new-syntax -Ysafe-init -feature -Xcheck-macros -Ycheck-all-patmat -Yexplicit-nulls -language:experimental.saferExceptions $SRC/*.scala && \
        jar cf "pub/$JAR" -C out "$PROJECT"
  fi
}

mkdir -p out pub

ALL="$(cat refs | cut -d: -f1)"
for REF in $ALL; do
  build $REF
done
