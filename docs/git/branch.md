# Função das Branches

## main

É a versão final e funcional distribuída ao público final.

O _merge_ só pode ser feito com a _branch_ **release**.

## hotfix

Usada quando aparece um erro na brach **main** que precisa ser resolvido urgentemente.
Após a solucionar e testar é feito um merge diretamente na **main**.

## release

Usada para receber tudo que foi desenvolvido até o momento antes que as alterações sejam enviadas para a **main**.

## test

Usada para testar as modificações e recursos. Nela é feita a procura por _bugs_ e validação dos resultados.

## developer

Recebe todas as mudanças feitas em todas as tarefas e as que foram feitas na _branch_ **main** e **release**.
É a partir dela que são criar as _branches_ **feat** e **style**.

## feat

Usada para o desenvolvimento de uma nova função ou recurso.

## style

Usada para alterações visuais na interface mas que não adicionam recursos.
