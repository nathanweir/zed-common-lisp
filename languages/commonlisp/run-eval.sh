#!/bin/sh

cat <<EOF > /tmp/cl_expr.lisp
$ZED_CUSTOM_CL_EVAL_EXPRESSION
EOF

# TODO: Consider making the client read from the file instead. However, while I thought this would fall down
# for double quotes, it doesn't seem to, so this might be OK?
/home/nathan/dev/alive-lsp/ros/alive-client "$(cat /tmp/cl_expr.lisp)"
