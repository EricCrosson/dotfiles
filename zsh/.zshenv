##
# Global PATH configuration
# golang
path=('/usr/local/go/bin' $path)
path=("${GOPATH}/bin" $path)

# rust
path=("${HOME}/.cargo/bin" $path)

# python
path+=("/usr/local/anaconda3/bin")
path+=("${HOME}/Library/Python/2.7/bin")
path+=("${HOME}/Library/Python/3.6/bin")

# ruby
path=("${HOME}/.gem/bin" $path)

# npm
path=("${HOME}/npm/bin" $path)

path=("${HOME}/bin" $path)
path=("${HOME}/bin/nix" $path)
path=("${HOME}/bin/darwin" $path)

# Path configuration
path+=('/sbin')
path+=('/usr/sbin')
path+=('/usr/local/sbin')
path+=('/bin')
path+=('/usr/bin')
path+=('/usr/local/bin')

export -U PATH
##

##
# Goss configuration
export GOSS_PATH=/usr/local/bin/goss
export GOPATH="${HOME}/workspace/golang"
##

##
# Python configuration
source "$(which virtualenvwrapper.sh)" 2>/dev/null
##

##
# Perl configuration
source ~/perl5/perlbrew/etc/bashrc
##
