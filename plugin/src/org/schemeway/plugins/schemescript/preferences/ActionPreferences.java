/*
 * Copyright (c) 2004 Nu Echo Inc.
 *
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;

public class ActionPreferences extends SchemePreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".action.";
    public final static String ACTION_PRIVATE_ALIAS = PREFIX + "private-alias";

    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(ACTION_PRIVATE_ALIAS, false);
    }

    public static boolean usePrivateAlias() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();

        return store.getBoolean(ActionPreferences.ACTION_PRIVATE_ALIAS);
    }

    private Button mPrivateAlias;

    @Override
    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(1, false));

        createCommandsSection(composite);

        initializeValues();
        return composite;
    }

    private void createCommandsSection(Composite parent) {
        Group group = new Group(parent, SWT.NONE);
        group.setText("Commands");
        group.setLayout(new GridLayout(1, false));
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_BEGINNING));

        mPrivateAlias = new Button(group, SWT.CHECK);
        mPrivateAlias.setText("Add define-alias command inserts define-private-alias");
    }

    @Override
    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    @Override
    protected void doPerformDefaults() {
        IPreferenceStore store = getPreferenceStore();

        mPrivateAlias.setSelection(store.getDefaultBoolean(ACTION_PRIVATE_ALIAS));
    }

    @Override
    protected void initializeValues() {
        IPreferenceStore store = getPreferenceStore();

        mPrivateAlias.setSelection(store.getBoolean(ACTION_PRIVATE_ALIAS));
    }

    @Override
    protected void storeValues() {
        IPreferenceStore store = getPreferenceStore();

        store.setValue(ACTION_PRIVATE_ALIAS, mPrivateAlias.getSelection());
    }
}