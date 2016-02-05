VERSION=$1

PATTERN_BUILD_SBT="s/(version[[:space:]]+:=[[:space:]]+)\".*\"/\1\"$VERSION\"/g"

PLUGINS_BUILD="plugins/build.sbt"
FRAMEWORK_BUILD="framework/build.sbt"
FRAMEWORK_PLUGIN="framework/project/subscript.sbt"
PLUGIN_SOURCE="plugins/plugin-parser/src/main/scala/subscript/plugin/SubscriptSbt.scala"

sed -E $PATTERN_BUILD_SBT $PLUGINS_BUILD > $PLUGINS_BUILD
sed -E $PATTERN_BUILD_SBT $FRAMEWORK_BUILD > $FRAMEWORK_BUILD

sed -E "s/(addSbtPlugin\(\"org.subscript-lang\" %% \"subscript-sbt-plugin\" % \").*(\"\))/\1$VERSION\2/g" $FRAMEWORK_PLUGIN > $FRAMEWORK_PLUGIN
sed -E "s/(addCompilerPlugin\(\"org.subscript-lang\" %% \"enhancedmacros\" % \").*(\"\))/\1$VERSION\2/g" $PLUGIN_SOURCE > $PLUGIN_SOURCE
