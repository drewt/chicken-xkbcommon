xkbcommon
=========

CHICKEN Scheme bindings for libxkbcommon.

Building

Dependencies:

* CHICKEN 5
* libxkbcommon

Run <code>chicken-install</code> in this directory to build and install the
bindings. You may need to manually copy the file
<code>xkb-common-keysyms.scm</code> into your CHICKEN include path as
<code>chicken-install</code> does not seem to install includes properly (at
least for me.

Usage
-----

    (import (xkbcommon))
    (import (xkbcommon compose))
    (include "xkbcommon-keysyms.scm")

Note that xkbcommon-keysyms should be included, rather than imported. This is
due to the large number of symbols that would have to be exported by a module.

### Naming Conventions

Procedures use the usual <code>kebab-case</code> convention.

Struct members are available as SRFI-17 getters/setters named
<code>struct-name-member-name</code>, e.g.
<code>xkb-rule-names-rules</code>.

Enums use the convention <code>enum-prefix/kind</code>, e.g.
<code>xkb-mod-name/shift</code>.
