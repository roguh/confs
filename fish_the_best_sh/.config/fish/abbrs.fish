if command -v git > /dev/null
    abbr ga 'git add'
    abbr gc 'git commit'
    abbr gch 'git checkout'
    abbr gchm 'git checkout main'
    abbr gs 'git status'
    abbr gst 'git stash push --'
    abbr gstp 'git stash pop'
    abbr gd 'git diff'
    abbr gdt 'git difftool'
    abbr gl 'git log'
    debug Setup Git abbreviations
end

if command -v kubectl > /dev/null
    abbr kx kubectx
    abbr kc 'kubectl config'
    # List and detail resources
    abbr kci 'kubectl cluster-info'
    abbr kgp 'kubectl get pods'
    abbr kgn 'kubectl get nodes'
    abbr kgs 'kubectl get services'
    abbr kd 'kubectl describe'
    abbr ke 'kubectl explain'
    # Debugging pods
    abbr kl 'kubectl logs'
    abbr kt 'kubectl top'
    abbr kcp 'kubectl cp'
    abbr kex 'kubectl exec'
    abbr kpf 'kubectl port-forward'
    debug Setup Kubernetes abbreviations
end

if command -v docker > /dev/null
    abbr dcls 'docker container ls'
    abbr dl 'docker logs'
    abbr da 'docker attach'
    abbr dex 'docker exec'
    abbr dck 'docker container kill (docker ps -q)'
    debug Setup Docker abbreviations
end

if command -v nvim > /dev/null
    abbr v 'nvim'
    abbr vmin 'nvim -u ~/.vimrc.minimal'
    debug Setup Neovim abbreviations
end
