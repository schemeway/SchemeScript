/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.schemeway.plugins.schemescript.*;

/**
 * @author Nu Echo Inc.
 */
public final class SchemeActionConstants {
    private SchemeActionConstants() {
    }

    private final static String SEXP_PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".sexp.";
    public final static String SEXP_FORWARD = SEXP_PREFIX + "forward";
    public final static String SEXP_SELECT_FORWARD = SEXP_PREFIX + "select-forward";
    public final static String SEXP_BACKWARD = SEXP_PREFIX + "backward";
    public final static String SEXP_SELECT_BACKWARD = SEXP_PREFIX + "select-backward";
    public final static String SEXP_UP = SEXP_PREFIX + "up";
    public final static String SEXP_SELECT_UP = SEXP_PREFIX + "select-up";
    public final static String SEXP_DOWN = SEXP_PREFIX + "down";
    public final static String SEXP_SWAP = SEXP_PREFIX + "swap";
    public final static String SEXP_FORMAT = SEXP_PREFIX + "format";
    public final static String SEXP_MOUSECOPY = SEXP_PREFIX + "mousecopy";
    public final static String SEXP_EXTENDED_MOUSECOPY = SEXP_PREFIX + "extended-mousecopy";
    public final static String SEXP_RESTORE_SELECTION = SEXP_PREFIX + "restore-selection";
    
    private final static String COMMENT_PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".comment.";
    public final static String COMMENT_CHAPTER = COMMENT_PREFIX + "chapter";
    public final static String COMMENT_SECTION = COMMENT_PREFIX + "section";
    public final static String COMMENT_HEADER  = COMMENT_PREFIX + "header";
    public final static String COMMENT_SELECTION = COMMENT_PREFIX + "selection";
    
    private final static String EVAL_PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".eval.";
    public final static String EVAL_EXPR = EVAL_PREFIX + "expression";
    public final static String EVAL_DEF = EVAL_PREFIX + "definition";
    public final static String EVAL_START = EVAL_PREFIX + "start";
    public final static String EVAL_RESTART = EVAL_PREFIX + "restart";
    public final static String EVAL_FAST = EVAL_PREFIX + "fast";
    public final static String EVAL_LOAD = EVAL_PREFIX + "load";
    
    private final static String FORMAT_PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".format.";
    public final static String COMPRESS_SPACES = FORMAT_PREFIX + "compress";
    
    public final static String JUMP_DEF = SchemeScriptPlugin.PLUGIN_NS + ".jump.definition";
    public final static String CHOOSE_SYMBOL = SchemeScriptPlugin.PLUGIN_NS + ".dictionary.find";
    
    
}
