#!/bin/sh

cat <<EOF > /tmp/cl_expr.lisp
$ZED_CUSTOM_CL_EVAL_EXPRESSION
EOF

# TODO: the tasks.json here is not even slightly portable as I've hardcoded paths on my system. Need to revisit
# this when I'm ready to fully publish this extension, but for now I just need eval to function for development

# TODO: Consider making the client read from the file instead. However, while I thought this would fall down
# for double quotes, it doesn't seem to, so this might be OK?
/home/nathan/dev/alive-lsp/ros/alive-client "$(cat /tmp/cl_expr.lisp)"
