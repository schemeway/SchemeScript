/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;

import org.schemeway.plugins.schemescript.*;

public class CommentPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".comment.";
    public final static String COMMENT_PREFIX = PREFIX + "prefix";
    public final static String COMMENT_AUTHOR = PREFIX + "author";
    public final static String COMMENT_COPYRIGHT = PREFIX + "copyright";
    public final static String COMMENT_CONTINUE = PREFIX + "continue";

    public CommentPreferences() {
        super("Scheme comment-related preferences", GRID);
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        initializeDefaults(store);
    }

    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(COMMENT_PREFIX, ";;");
        store.setDefault(COMMENT_AUTHOR, "");
        store.setDefault(COMMENT_COPYRIGHT, "");
        store.setDefault(COMMENT_CONTINUE, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        addField(new StringFieldEditor(COMMENT_PREFIX, "Prefix for comments:", getFieldEditorParent()));
        addField(new StringFieldEditor(COMMENT_AUTHOR, "Author field content:", getFieldEditorParent()));
        addField(new StringFieldEditor(COMMENT_COPYRIGHT, "Copyright field content:", getFieldEditorParent()));
        addField(new BooleanFieldEditor(COMMENT_CONTINUE,
                                        "Automatically continue comment from previous line",
                                        getFieldEditorParent()));
    }

    public static String getCommentPrefix() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        String prefix = store.getString(CommentPreferences.COMMENT_PREFIX);
        if (prefix == null || prefix.equals("")) {
            return ";";
        }
        else {
            return prefix;
        }
    }
}