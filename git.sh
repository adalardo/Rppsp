git status
#git add .
git add R/odk.R
git add R/mapsvg.R
git add git.sh
#git reset
#git add .
git status
#git add R/connMysql.R R/floraBrasilname.R R/spcode.R R/spname.R R/wdglobal.rda R/wdplot.R man/connMysql.Rd
#git add data
git commit -m "mapa da sequencia de arvores de auditoria modificado"
# ! [rejected]        master -> master (fetch first)
git branch
## deve estar em master
git remote -v
git push origin master
adalardo
gitale133113
#### para olhar diferencas entre branchs
gitk --all ## olhar todas as mundancas em um gui
git mergetool # usar o meld para merge diff

## incorporando mudancas do remote no local
git remote -v
git fetch origin
git merge origin master
## ou
git branch
git checkout master
git pull

## olhando os arquivos modificados
git ls-files --stage
git status --porcelain
git diff-files

## Problemas no commit
# error: failed to push some refs to 'https://github.com/adalardo/Rppsp.git'
# hint: Updates were rejected because the remote contains work that you do
# hint: not have locally. This is usually caused by another repository pushing
# hint: to the same ref. You may want to first integrate the remote changes
# hint: (e.g., 'git pull ...') before pushing again.
# hint: See the 'Note about fast-forwards' in 'git push --help' for details.
git branch -v
git remote
git pull
git mergetool

