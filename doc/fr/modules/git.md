- ## Git
+ ## Git

- ### About
+ ### Présentation

- [Git](https://git-scm.com/) is driven through typed operations on a repository which is opened
- for a scope. A branch, a tag and a commit hash are distinct types, not interchangeable strings.
- Results come back as values, not as text to parse. And the API is honest about the one thing
- every Git library must be honest about: the repository is on disk, and other processes write to
- it.
+ [Git](https://git-scm.com/) se pilote par des opérations typées sur un dépôt ouvert pour une
+ portée. Une branche, un tag et un hachage de commit sont des types distincts, et non des
+ chaînes interchangeables. Les résultats reviennent sous forme de valeurs, et non de texte à
+ analyser. Et l'API est honnête sur la seule chose dont toute bibliothèque Git doit être
+ honnête : le dépôt est sur disque, et d'autres processus y écrivent.

- A backend carries out the operations. The command-line backend runs the `git` command, so
- anything Git can do is available. The native backend reads and writes the files of the
- repository itself, and needs no `git` at all. Both implement the same `Git.Backend`
- [typeclass], and the choice is an import: `gitBackends.commandlineGit` or
- `gitBackends.soundnessGit`.
+ Un backend réalise les opérations. Le backend en ligne de commande exécute la commande `git` ;
+ tout ce que Git sait faire est donc disponible. Le backend natif lit et écrit lui-même les
+ fichiers du dépôt, et n'a nul besoin de `git`. Tous deux implémentent la même [classe de types]
+ `Git.Backend`, et le choix se fait par un import : `gitBackends.commandlineGit` ou
+ `gitBackends.soundnessGit`.

- ### On Git
+ ### Sur Git

- Git's real interface is its command line, built for a person at a shell. Driving it from a
- program means assembling command strings, parsing text output by hand, and treating a branch
- name, a tag name and a commit hash alike until Git rejects one. Worse, every program which
- drives Git races with every other. A ref read now may have moved by the time it is written.
+ La véritable interface de Git est sa ligne de commande, conçue pour une personne devant un
+ shell. La piloter depuis un programme signifie assembler des chaînes de commande, analyser à la
+ main une sortie textuelle, et traiter de la même façon un nom de branche, un nom de tag et
+ un hachage de commit jusqu'à ce que Git en rejette un. Pire, chaque programme qui pilote Git est
+ en concurrence avec tous les autres. Une référence lue maintenant peut avoir bougé au moment où
+ on l'écrit.

- Repository operations here report their failures in their types, cannot run without the
- [capabilities][capability] they need, and cannot pretend that a reading is still current. These
- are [honest signatures](../../philosophy/honest-signatures.md) for version control.
+ Ici, les opérations sur un dépôt signalent leurs échecs dans leurs types, ne peuvent s'exécuter
+ sans les [capacités][capacité] dont elles ont besoin, et ne peuvent prétendre qu'une lecture est
+ toujours d'actualité. Ce sont des [signatures honnêtes](../../philosophy/honest-signatures.md)
+ pour la gestion de versions.

- Everything comes from the `soundness` package. The other imports select a backend, bring the
- capabilities which the operations need into scope, and let the samples print their results. The
- samples work in a fresh repository under the temporary directory:
+ Tout provient du package `soundness`. Les autres imports choisissent un backend, apportent dans
+ la portée les capacités dont les opérations ont besoin, et permettent aux exemples d'afficher
+ leurs résultats. Les exemples travaillent dans un nouveau dépôt sous le répertoire temporaire :

```scala
import soundness.*

import codepages.utf8Codepage
import filesystemOptions.createNonexistentParents
import gitBackends.commandlineGit
import gitCommands.searchpathGitCommand
import internetAccess.online
import logging.silentLogging
import pathInterfaces.pathOnLocal
import systems.javaBaseSystem
import strategies.throwUnsafely
import temporaryDirectories.javaBaseTemporaryDirectory
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
import filesystemBackends.javaBaseFilesystem
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val directory = temporaryDirectory[Path on Local] / "git" / Uuid().show
directory.create[Directory](CreateFlag.Parents)
```

- ### Opening a repository
+ ### Ouvrir un dépôt

- A repository is created with `Git.init`, and is opened, for the duration of a block, as one of
- two forms. A `Git.Repo` is the object
- database and its refs, which is what a bare repository has. A `Git.Worktree` is a checked-out
- tree, with an index and working files, over its repository.
+ Un dépôt se crée avec `Git.init`, et s'ouvre, pour la durée d'un bloc, sous l'une de deux
+ formes. Un `Git.Repo` est la base d'objets et ses références, ce dont dispose un dépôt nu. Un
+ `Git.Worktree` est un arbre extrait, avec un index et des fichiers de travail, au-dessus de son
+ dépôt.

- The mode names the grants which the handle carries. A handle opened with `Read` alone cannot be
- used to write, and that is a compile error, not a runtime one:
+ Le mode nomme les autorisations que porte la poignée. Une poignée ouverte avec `Read` seul ne
+ peut servir à écrire, et c'est une erreur de compilation, et non d'exécution :

```scala fuel
run
##
Git.init(directory, initialBranch = Git.Branch(t"main"))

directory.open[Git.Worktree](Read & Write): worktree ?=>
  Out.println(worktree.head().value.inspect)
```

- The repository has no commits yet, so `HEAD` is attached to a branch with no tip.
+ Le dépôt n'a pas encore de commits ; `HEAD` est donc attaché à une branche sans sommet.

- A `Git.Worktree.Handle` is also a `Git.Repo.Handle`, so every repository operation is available
- on it. It is a directory handle too. A path formed from it, such as `worktree / "notes.txt"`,
- belongs to this handle alone, cannot express `..`, and may be read and written with the
- [filesystem](filesystem.md) operations under the same grants.
+ Une `Git.Worktree.Handle` est aussi une `Git.Repo.Handle` ; toute opération sur un dépôt y est
+ donc disponible. C'est aussi une poignée de répertoire. Un chemin formé à partir d'elle, comme
+ `worktree / "notes.txt"`, n'appartient qu'à cette poignée, ne peut exprimer `..`, et peut être
+ lu et écrit avec les opérations du [système de fichiers](filesystem.md) sous les mêmes
+ autorisations.

- ### Observing a repository that others change
+ ### Observer un dépôt que d'autres modifient

- Every read which resolves a ref, `HEAD`, the index or the working files returns a `Stale` value.
- What it holds was true at the moment of the read, and may have changed since. `head()` reads
- where `HEAD` is, and the `Stale[Git.Head]` it returns is the proof of what was seen:
+ Chaque lecture qui résout une référence, `HEAD`, l'index ou les fichiers de travail renvoie une
+ valeur `Stale`. Ce qu'elle contient était vrai au moment de la lecture, et peut avoir changé
+ depuis. `head()` lit où se trouve `HEAD`, et le `Stale[Git.Head]` qu'il renvoie est la preuve de
+ ce qui a été vu :

```scala fuel
run
##
directory.open[Git.Worktree](): worktree ?=>
  val head: Stale[Git.Head] = worktree.head()

  head.value match
    case Git.Head.Attached(branch, tip) => Out.println(t"on $branch")
    case Git.Head.Detached(at)          => Out.println(t"detached at $at")
```

- Every write which moves a ref takes the `Stale` value it is conditional on. It fails with
- `Git.Error(Moved(ref, expected, actual))` if the ref no longer holds that value. This is Git's
- own compare-and-swap, `git update-ref ref new old`, made unavoidable. A commit cannot overwrite
- work which another process committed between the read and the write.
+ Chaque écriture qui déplace une référence prend la valeur `Stale` dont elle dépend. Elle échoue
+ avec `Git.Error(Moved(ref, expected, actual))` si la référence ne contient plus cette valeur.
+ C'est le compare-and-swap propre à Git, `git update-ref ref new old`, rendu incontournable. Un
+ commit ne peut écraser un travail qu'un autre processus a commité entre la lecture et
+ l'écriture.

- ### Making a commit
+ ### Faire un commit

- Staging and committing are methods on the worktree. `commit` takes the reading of `HEAD` it is
- conditional on, and returns the hash of the new commit:
+ L'indexation et le commit sont des méthodes du worktree. `commit` prend la lecture de `HEAD` dont
+ il dépend, et renvoie le hachage du nouveau commit :

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  val head = worktree.head()
  (worktree / "notes.txt").overwrite(t"Remember the milk")
  worktree.add(worktree / "notes.txt")
  val hash = worktree.commit(t"Add notes", head)
  Out.println(hash.show.length.show)
```

- The commit object is assembled here and stored, and is not made by `git commit`. So a signature
- (`signer = …`) never leaves the process, and the same code serves both backends. A commit whose
- tree is unchanged raises `NothingToCommit`, unless `allowEmpty = true`. `amend` rewrites the
- tip.
+ L'objet commit est assemblé et stocké ici, et n'est pas créé par `git commit`. Ainsi, une
+ signature (`signer = …`) ne quitte jamais le processus, et le même code sert les deux backends.
+ Un commit dont l'arbre est inchangé lève `NothingToCommit`, sauf avec `allowEmpty = true`.
+ `amend` réécrit le sommet.

- ### Inspecting history
+ ### Inspecter l'historique

- `log` gives the commits as a list, newest first. Each is a `Commit` with its hash, author,
- parents and message. The list is `Stale`, since the branch it follows may move:
+ `log` donne les commits sous forme de liste, du plus récent au plus ancien. Chacun est un
+ `Commit` avec son hachage, son auteur, ses parents et son message. La liste est `Stale`, puisque
+ la branche qu'elle suit peut bouger :

```scala fuel
run
##
directory.open[Git.Worktree](): worktree ?=>
  worktree.log().value.each(commit => Out.println(commit.message))
```

- Objects are [immutable] and named by their content, so a read keyed by a hash is a plain value:
+ Les objets sont [immuables][immuable] et nommés par leur contenu ; une lecture indexée par un
+ hachage est donc une simple valeur :

```tabular
- column  A read of
- column  Such as
- column  Returns
- row  a ref, HEAD, the index or the working files  `head`, `ref`, `resolve`, `branches`, `tags`, `log`, `note`, `remotes`, `stashes`, `diff`  a `Stale` value
- row  an object, by its hash  `show`, `commitAt`, `treeAt`, `blobAt`, `diffBetween`  a plain value
+ column  Une lecture de
+ column  Comme
+ column  Renvoie
+ row  une référence, HEAD, l'index ou les fichiers de travail  `head`, `ref`, `resolve`, `branches`, `tags`, `log`, `note`, `remotes`, `stashes`, `diff`  une valeur `Stale`
+ row  un objet, par son hachage  `show`, `commitAt`, `treeAt`, `blobAt`, `diffBetween`  une simple valeur
```

- ### Branches and tags
+ ### Branches et tags

- Branches and tags are created, listed and switched between with typed references. A branch is
- created at a commit, and switching takes the reading of `HEAD` it moves from:
+ Les branches et les tags se créent, se listent et s'alternent avec des références typées.
+ Une branche se crée à un commit, et le changement de branche prend la lecture de `HEAD` à partir
+ de laquelle il se déplace :

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
  worktree.switch(Git.Branch(t"main"), worktree.head())
  worktree.branches().value.keys.each(branch => Out.println(branch.show))
```

- Deleting a branch or a tag takes the reading of its tip, so a branch which moved since it was
- listed is not deleted by mistake. `makeTag` makes a lightweight tag. `annotatedTag` writes a tag
- object with a message, and optionally a signature.
+ Supprimer une branche ou un tag prend la lecture de son sommet ; une branche qui a bougé
+ depuis qu'elle a été listée n'est donc pas supprimée par erreur. `makeTag` crée un tag
+ léger. `annotatedTag` écrit un objet tag avec un message, et éventuellement une
+ signature.

- ### Cloning, fetching and pushing
+ ### Cloner, récupérer et pousser

- An operation which reaches a remote needs the `Online` capability. It blocks until it is done,
- and reports its progress through a `Git.Activity`: a shared tracker of atomic counters, updated
- as objects arrive, which a user interface may poll from another thread with `report()`.
+ Une opération qui atteint un dépôt distant a besoin de la capacité `Online`. Elle bloque jusqu'à
+ sa fin, et signale sa progression par une `Git.Activity` : un suivi partagé de compteurs
+ atomiques, mis à jour à l'arrivée des objets, qu'une interface utilisateur peut interroger depuis
+ un autre thread avec `report()`.

```scala
import dysasymptotics.linearSize

val target = directory / "clone"
Git.clone(directory, target)
```

```scala fuel
run
##
target.open[Git.Worktree](): cloned ?=>
  Out.println(cloned.log().value.size.show)
```

- `push` takes refspecs and a force policy. `Push.Force.Lease` is the compare-and-swap on the
- remote side, `--force-with-lease`. It carries the readings of the remote's refs which the push
- is conditional on.
+ `push` prend des refspecs et une politique de forçage. `Push.Force.Lease` est le compare-and-swap
+ du côté distant, `--force-with-lease`. Elle porte les lectures des références distantes dont
+ dépend le push.

- ### Choosing a backend
+ ### Choisir un backend

- The two backends differ in how they do the work, and so in what each is for:
+ Les deux backends diffèrent dans leur façon de travailler, et donc dans ce à quoi chacun sert :

```tabular
- column  Backend
- column  Works by
- column  Reaches remotes
- column  The choice for
- row  `gitBackends.commandlineGit`  running the `git` command  as `git` does  git's own behaviour in full: hooks, filters, credential helpers, submodules
- row  `gitBackends.soundnessGit`  reading and writing git's formats directly  through a transport: `SmartHttp`, `SshCommand`  no dependency on `git`
+ column  Backend
+ column  Fonctionne en
+ column  Atteint les dépôts distants
+ column  Le choix pour
+ row  `gitBackends.commandlineGit`  exécutant la commande `git`  comme le fait `git`  le comportement complet de git : hooks, filtres, assistants d'identification, sous-modules
+ row  `gitBackends.soundnessGit`  lisant et écrivant directement les formats de git  par un transport : `SmartHttp`, `SshCommand`  aucune dépendance à `git`
```

- `gitBackends.commandlineGit` runs the `git` command which `gitCommands.searchpathGitCommand`
- finds, in the working directory and environment in scope, and logs each invocation.
+ `gitBackends.commandlineGit` exécute la commande `git` que trouve
+ `gitCommands.searchpathGitCommand`, dans le répertoire de travail et l'environnement de la
+ portée, et journalise chaque invocation.

- `gitBackends.soundnessGit` is the native backend. Objects, loose and packed, refs, the index,
- configuration, notes, linked worktrees, merging, rebasing and the stash are all read and
- written directly, in Git's own formats. A repository is therefore always one which `git` itself
- understands, and an operation which one side stops on a conflict, the other can continue. It
- reaches a repository on the same filesystem by path. A remote one needs a transport, which the
- `http` and `ssh` components supply. `octogenarian.Native(List(SmartHttp(), SshCommand()))`
- speaks Git's wire protocol over whichever `Http.Backend` is in scope, and over a spawned `ssh`.
- `Native` is not exported to `soundness`, where the name is xenophile's, so it is written
- qualified:
+ `gitBackends.soundnessGit` est le backend natif. Les objets, libres et empaquetés, les
+ références, l'index, la configuration, les notes, les worktrees liés, la fusion, le rebasage et
+ le stash sont tous lus et écrits directement, dans les formats propres de Git. Un dépôt est donc
+ toujours un dépôt que `git` lui-même comprend, et une opération que l'un des côtés arrête sur un
+ conflit, l'autre peut la poursuivre. Il atteint un dépôt du même système de fichiers par son
+ chemin. Un dépôt distant exige un transport, que fournissent les composants `http` et `ssh`.
+ `octogenarian.Native(List(SmartHttp(), SshCommand()))` parle le protocole réseau de Git sur le
+ `Http.Backend` présent dans la portée, et sur un `ssh` lancé en sous-processus. `Native` n'est
+ pas exporté vers `soundness`, où le nom appartient à xenophile ; il s'écrit donc qualifié :

```scala fuel
typecheck
##
import gitTransports.{smartHttp, sshCommand}
import httpBackends.javaNetHttp

given Git.Backend = octogenarian.Native(List(SmartHttp(), SshCommand()))
```

- Signing is independent of the backend. A `Git.Signer` makes the signature which a commit or tag
- carries, and a `Git.Verifier` checks one:
+ La signature est indépendante du backend. Un `Git.Signer` produit la signature que porte un
+ commit ou un tag, et un `Git.Verifier` en vérifie une :

```tabular
- column  Scheme
- column  Signer
- column  Verifier
- column  Drives
- row  SSH  `SshKeygen.Signer(key)`  `SshKeygen.Verifier(allowedSigners)`  `ssh-keygen -Y`, as `gpg.format = ssh` has git do
- row  OpenPGP  `Gpg.Signer()`  `Gpg.Verifier()`  `gpg`
+ column  Schéma
+ column  Signataire
+ column  Vérificateur
+ column  Pilote
+ row  SSH  `SshKeygen.Signer(key)`  `SshKeygen.Verifier(allowedSigners)`  `ssh-keygen -Y`, comme le fait faire `gpg.format = ssh` à git
+ row  OpenPGP  `Gpg.Signer()`  `Gpg.Verifier()`  `gpg`
```

- A tag signed through either is one which `git tag -v` accepts.
+ Un tag signé par l'un ou l'autre est acceptée par `git tag -v`.

- Whichever backend is chosen, `Exclusive` arbitrates only between handles in this JVM. Another
- process is never waited for, which is why every operation which moves a ref is a compare-and-swap
- on the `Stale` reading it was given.
+ Quel que soit le backend choisi, `Exclusive` n'arbitre qu'entre les poignées de cette JVM. Un
+ autre processus n'est jamais attendu ; c'est pourquoi chaque opération qui déplace une référence
+ est un compare-and-swap sur la lecture `Stale` qui lui a été donnée.

- ### References
+ ### Références

- Each kind of reference is its own type:
+ Chaque sorte de référence est son propre type :

```tabular
- column  Type
- column  Names
- row  `Git.Branch`  a branch
- row  `Git.Tag`  a tag
- row  `Git.Hash`  a commit, by a well-formed forty-character identifier
- row  `Git.Revision`  any of these, or a relative expression: `Git.Revision.head(1)` is `HEAD~1`
- row  `Git.Ref`  a fully-qualified reference path, such as `refs/heads/main`
+ column  Type
+ column  Nomme
+ row  `Git.Branch`  une branche
+ row  `Git.Tag`  un tag
+ row  `Git.Hash`  un commit, par un identifiant bien formé de quarante caractères
+ row  `Git.Revision`  l'un de ceux-ci, ou une expression relative : `Git.Revision.head(1)` est `HEAD~1`
+ row  `Git.Ref`  un chemin de référence pleinement qualifié, comme `refs/heads/main`
```

- So an operation which expects a branch will not accept a tag:
+ Ainsi, une opération qui attend une branche n'acceptera pas un tag :

```scala fuel
compile
errors show
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.switch(Git.Tag(t"v1"), worktree.head())
```

- A `Git.Ref` is modeled as a [path](paths.md) on the `Git.Refs` plane. Reference names follow
- Git's own rules: no leading dot, no `..`, no trailing `lock` suffix, and the rest. The rules are
- checked when a name is parsed from text, with `Git.Branch.parse`, `Git.Tag.parse` or
- `Git.Refs.heads`, and a bad name is reported as a `Git.RefError`:
+ Une `Git.Ref` est modélisée comme un [chemin](paths.md) sur le plan `Git.Refs`. Les noms de
+ référence suivent les règles propres de Git : pas de point initial, pas de `..`, pas de suffixe
+ final `lock`, et le reste. Les règles sont vérifiées quand un nom est analysé à partir de texte,
+ avec `Git.Branch.parse`, `Git.Tag.parse` ou `Git.Refs.heads`, et un nom incorrect est signalé
+ sous forme de `Git.RefError` :

```scala fuel
run
errors show
##
Git.Branch.parse(t"feature..old")
```

- ### Diffs
+ ### Différences

- `diff` reports changes as structured values, not as patch text to parse again. Each
- `Git.Delta` carries the paths on both sides, the kind of change, which is added, modified,
- deleted, renamed or copied, and its hunks. Each hunk is a list of edits:
+ `diff` signale les modifications sous forme de valeurs structurées, et non de texte de patch à
+ analyser de nouveau. Chaque `Git.Delta` porte les chemins des deux côtés, la nature de la
+ modification, à savoir ajout, modification, suppression, renommage ou copie, et ses blocs.
+ Chaque bloc est une liste de modifications :

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  (worktree / "notes.txt").overwrite(t"Remember the milk and the eggs")
  val files = worktree.diff().value
  Out.println(files.head.let(_.status).inspect)
```

- Each diff method compares two sides:
+ Chaque méthode de différence compare deux côtés :

```tabular
- column  Method
- column  Left
- column  Right
- row  `diff()`  the index  the working files
- row  `diffStaged()`  HEAD  the index
- row  `diffAgainst(revision)`  the revision  the working files
- row  `diffBetween(left, right)`  a commit  a commit
+ column  Méthode
+ column  Gauche
+ column  Droite
+ row  `diff()`  l'index  les fichiers de travail
+ row  `diffStaged()`  HEAD  l'index
+ row  `diffAgainst(revision)`  la révision  les fichiers de travail
+ row  `diffBetween(left, right)`  un commit  un commit
```

- The same parser, `Git.Patch.parse`, reads a patch from anywhere.
+ Le même analyseur, `Git.Patch.parse`, lit un patch de n'importe quelle provenance.

- ### Merging, cherry-picking, reverting and rebasing
+ ### Fusion, cherry-pick, revert et rebasage

- `merge` takes the revision to merge, the reading of `HEAD`, and a fast-forward policy. Whether a
- merge commit is created is stated, and not inherited from configuration. The outcome is a
- value:
+ `merge` prend la révision à fusionner, la lecture de `HEAD`, et une politique d'avance rapide.
+ La création ou non d'un commit de fusion est énoncée, et non héritée de la configuration. Le
+ résultat est une valeur :

```tabular
- column  Outcome
- column  Meaning
- row  `FastForwarded(head)`  the branch moved forward to `head`, with no merge commit
- row  `Merged(head)`  a merge commit, `head`, was created
- row  `UpToDate`  there was nothing to merge
- row  `Stopped(operation)`  the merge stopped on a conflict
+ column  Résultat
+ column  Signification
+ row  `FastForwarded(head)`  la branche a avancé jusqu'à `head`, sans commit de fusion
+ row  `Merged(head)`  un commit de fusion, `head`, a été créé
+ row  `UpToDate`  il n'y avait rien à fusionner
+ row  `Stopped(operation)`  la fusion s'est arrêtée sur un conflit
```

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  Out.println(worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Only).inspect)
```

- A conflict is not an error. The operation stops, and the `Stopped` outcome carries a
- `Git.Operation` which names the paths in conflict. Once they are resolved and added,
- `continue()` finishes the operation, `skip()` passes over the commit, and `abandon()` restores
- the state the operation began from. `cherryPick`, `revert` and `rebase` have the same shape, and
- `stash` saves and restores uncommitted work.
+ Un conflit n'est pas une erreur. L'opération s'arrête, et le résultat `Stopped` porte une
+ `Git.Operation` qui nomme les chemins en conflit. Une fois ceux-ci résolus et ajoutés,
+ `continue()` termine l'opération, `skip()` passe le commit, et `abandon()` restaure l'état à
+ partir duquel l'opération a commencé. `cherryPick`, `revert` et `rebase` ont la même forme, et
+ `stash` met de côté et restaure le travail non commité.

- ### Remotes, configuration, notes and the reflog
+ ### Dépôts distants, configuration, notes et reflog

- Remotes are added, listed and removed as values. Configuration is read and written through a
- lent handle, typed by the codec of the value: `repo.config { config ?=> config.get[Text](key) }`.
+ Les dépôts distants s'ajoutent, se listent et se suppriment sous forme de valeurs. La
+ configuration se lit et s'écrit par une poignée prêtée, typée par le codec de la valeur :
+ `repo.config { config ?=> config.get[Text](key) }`.

- Notes attach text to a commit without altering it, in a chosen namespace. A note write is
- conditional on a reading of the notes ref. That is how build attestations and other out-of-band
- metadata travel with a repository, without one writer clobbering another:
+ Les notes attachent du texte à un commit sans le modifier, dans un espace de noms choisi.
+ L'écriture d'une note dépend d'une lecture de la référence des notes. C'est ainsi que les
+ attestations de build et autres métadonnées hors bande voyagent avec un dépôt, sans qu'un
+ écrivain n'écrase le travail d'un autre :

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.addRemote(t"origin", t"git@example.com:globe/hamlet.git")
  val hash = worktree.head().value.commit.or(Git.Hash.zero)
  val notes = worktree.ref(Git.Refs.defaultNotes)
  worktree.addNote(hash, t"a note body", notes)
  Out.println(worktree.note(hash).value.inspect)
```

- `note` gives `Unset` where there is no note. `reflog` gives the local history of where a
- reference has pointed, newest first. That record is what makes a mistaken reset recoverable.
+ `note` donne `Unset` là où il n'y a pas de note. `reflog` donne l'historique local des
+ emplacements vers lesquels une référence a pointé, du plus récent au plus ancien. Cette trace est
+ ce qui permet de rattraper une réinitialisation erronée.

- ### Related
+ ### Voir aussi

- - [Filesystem](filesystem.md) describes the directory handles and grants which a worktree
-   shares.
- - [Paths](paths.md) describes the planes which `Git.Ref` is a path on.
- - [Diffing](diffing.md) computes and applies diffs outside a repository.
- - [Processes](processes.md) describes how the command-line backend runs `git`.
- - [SSH](ssh.md) and the [HTTP client](http-client.md) carry the native backend's transports.
- - [Cryptography](cryptography.md) describes the keys which sign commits and tags.
- - [Trees](trees.md) draws a commit history in lanes.
+ - [Système de fichiers](filesystem.md) décrit les poignées de répertoire et les autorisations
+   que partage un worktree.
+ - [Chemins](paths.md) décrit les plans sur lesquels `Git.Ref` est un chemin.
+ - [Différences](diffing.md) calcule et applique des différences hors d'un dépôt.
+ - [Processus](processes.md) décrit comment le backend en ligne de commande exécute `git`.
+ - [SSH](ssh.md) et le [client HTTP](http-client.md) portent les transports du backend natif.
+ - [Cryptographie](cryptography.md) décrit les clés qui signent les commits et les tags.
+ - [Arbres](trees.md) dessine un historique de commits en couloirs.
