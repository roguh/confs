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
    abbr kgp 'kubectl get pods'
    abbr kd 'kubectl describe'
    # Debugging pods
    abbr kl 'kubectl logs'
    abbr kcp 'kubectl cp'
    abbr kex 'kubectl exec'
    abbr kpf 'kubectl port-forward'
    debug Setup Kubernetes abbreviations
end

if command -v docker > /dev/null
    abbr dcls 'docker container ls'
    abbr dl 'docker logs'
    abbr dex 'docker exec'
    abbr dck 'docker container kill (docker ps -q)'
    debug Setup Docker abbreviations
end
