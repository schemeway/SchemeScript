/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.swt.graphics.*;

public interface ISchemeColorConstants {
    RGB SCHEME_COMMENT = new RGB(0, 128, 0);
    RGB SCHEME_CONSTANT = new RGB(92, 0, 0);
    RGB SCHEME_DEFINE = new RGB(0, 0, 0);
    RGB SCHEME_KEYWORD = new RGB(50, 128, 50);
    RGB SCHEME_SPECIAL = new RGB(0, 0, 255);
    RGB SCHEME_MUTATOR = new RGB(196, 0, 0);
    RGB SCHEME_TYPE = new RGB(128, 0, 128);
    RGB SCHEME_ERROR = new RGB(255, 0, 0);

    RGB SCHEME_STRING = new RGB(96, 96, 96);
    RGB SCHEME_DEFAULT = new RGB(0, 0, 0);

    RGB SCHEME_BACKGROUND = new RGB(255, 255, 255);

    RGB MATCHING_PARENS = new RGB(128, 196, 128);
}