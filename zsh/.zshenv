source ~/perl5/perlbrew/etc/bashrc

##
# Path configuration
path+=('/usr/local/bin')
path+=('/usr/bin')
path+=('/bin')
path+=('/usr/local/sbin')
path+=('/usr/sbin')
path+=('/sbin')

# golang
path+=('/usr/local/go/bin')

# rust
path=("${HOME}/.cargo/bin" $path)

# python
path=("${HOME}/Library/Python/2.7/bin" $path)
path=("${HOME}/Library/Python/3.6/bin" $path)
path=("/usr/local/anaconda3/bin" $path)

# ruby
path=("${HOME}/.gem/bin" $path)

# npm
path=("${HOME}/npm/bin" $path)

path=("${HOME}/bin" $path)
path=("${HOME}/bin/nix" $path)
path=("${HOME}/bin/darwin" $path)

export -U PATH
##
