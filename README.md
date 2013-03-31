flexoplan
=========

Planner (TODO list), that it flexible. Very alpha version, so any bug-reports are highly welcome.

Consists of 2 parts: CL-FLEXOPLAN Common Lisp server and Emacs flexoplan-mode frontend.
It is assumed, that Emacs + Quicklisp + Slime is configured.
It is further assumed, that MySQL server (needed for persistence) is running on a local machine on a standard socket.

This being the case, when inside the Emacs, issue the following:

        M-x slime
        CL-USER> (ql:quickload 'cl-flexoplan)
        CL-USER> (cl-flexoplan:start-server) ; server will be started on port 4006
        C-cC-f ; this will bring you to *flexoplan* buffer, which will be already connected to CL-FLEXOPLAN backend.

To add a goal, say, "do foobar" to the plan, simply write on any convenient line (in *flexoplan* buffer):

        * do foobar

To change goal status to and from "done", type "C-cC-c", when cursor is on the line you want to mark/unmark "done".
To load from a database goals, belonging to a specific project, type "C-cp". You'll be prompted for the project name.
Leave that blank, to load all goals.

To display only not-done goals in a given project, type "C-cs".
To display all the goals in a given project, type "C-cS".

To send changes, made to *flexoplan* buffer to CL backend, type "C-xC-s". They will be saved both in local
hash-table, and in the database.

Finally, "C-cC-f" keybinding is "magical". When you are not in *flexoplan* buffer, it will bring you to it,
establishing connection with a backend, if it's not already done. If, however, you already are in *flexoplan* buffer,
it will bring you to the buffer you were editing before *flexoplan*.

The whole idea of the project is to make planning and tracking of one's progress as unhindering
to the process of Creation, as possible.

TODO (of course, in the notation, that flexoplan understands, or will understand):

        * persistence
          * ability to specify host/port, where MySQL lives
          * ability to turn off persistence altogether
          * ability to specify MySQL user/password
          * version control
            - when, and what changes to plan were made
        * tree structure of a plan
        * description field for each goal
          - This field should be somewhat elaborate description of a problem specified by the title.
            It might also include references to subgoals through titles like this
            {toggle display of descriptions on/off} and
            {compile "essay" from descriptions of goals}.
          * toggle display of descriptions on/off
          * compile "essay" from descriptions of goals
            - One of the secondary "aims" of this project is to be a mode in which it is easy to write essays.
              Each thought starts out as a title of "goal", and then is expanded in the description.
              While writing the description for some goal (i.e., thinking about it), the ideas of new
              interesting thoughts arise. These can be immediately referred to in the text of description being
              written, and automatically created. Later, they again can be expanded with help of their
              description field.
              Finally, the "flat" text is "compiled" from this tree-like structure, ready to be published
              on the web or in some article.
              Precisely having this purpose in mind, the [version control] goal (square brackets will denote
              refs to goals whose descriptions will not be compiled-in) is important.
        * more secure Emacs-CL channel
          - now, when CL-FLEXOPLAN server runs, practically everyone on a localhost can connect to it,
            and execute any lisp command, which is rather not nice.
            Should develop some kind of "restricted" swank-shell, in which
            * read-eval is disabled
            * only calls to certain functions are allowed
            Then, securing would simply mean to restrict this set of allowed funcs, to the ones, say,
            exported from CL-FLEXOPLAN package.
        * support for unicode
          - I simply do not know, whether it works, should test it.
        * other frontends
          * web
            - the main difficulty here is to determine, to what extent this frontend
              should provide Emacs-like editing facilities. 


