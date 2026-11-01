# publish-config.sh --- sourced by publish.sh and publish-hub.sh. Where a
# release goes is the publishing machine's to know, never the source's: each
# setting is read from the environment, else from the local publish config
# (NAME=value lines, the last one winning), and a setting found in neither
# stops the publish before anything is sent.
publish_config="${NODECODE_PUBLISH_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/nodecode/publish.env}"
publish_setting() {
  local name="$1" value="${!1:-}"
  if [ -z "$value" ] && [ -f "$publish_config" ]; then
    value="$(sed -n "s/^$name=//p" "$publish_config" | tail -n 1)"
  fi
  if [ -z "$value" ]; then
    echo "publish: $name is not set: export it, or add $name=<value> to $publish_config" >&2
    return 1
  fi
  printf '%s\n' "$value"
}
