/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;

public class SchemePreferences extends SchemePreferencePage {

    private final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".editor.";
    public final static String TAB_WIDTH = PREFIX + "tabWidth";
    
    public final static int DEFAULT_TABWIDTH = 4;
    private final static int MIN_TABWIDTH = 1;
    private final static int MAX_TABWIDTH = 200;
    
    private int mTabWidth; 
    
    protected Control createContents(Composite parent) {
        initializeValues();
        Composite composite = new Composite(parent, SWT.NULL);
        composite.setLayout(new GridLayout(1, false));
        composite.setLayoutData(new GridData(GridData.FILL_BOTH));

        createTabWidthControl(composite);
        createSerializationControl(composite);

        return getControl();
    }

    private void createSerializationControl(Composite composite) {
        Group serializeGroup = new Group(composite, SWT.SHADOW_ETCHED_OUT);
        serializeGroup.setText("Import/Export");
        serializeGroup.setLayout(new GridLayout(2, false));
        serializeGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_BEGINNING));

        Label label = new Label(serializeGroup, SWT.NONE);
        label.setText("Save or load your Scheme preferences.");
        GridData data = new GridData(GridData.FILL_HORIZONTAL);
        data.horizontalSpan = 2;
        label.setLayoutData(data);

        Button exportButton = new Button(serializeGroup, SWT.PUSH);
        exportButton.setText("Save...");
        exportButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                PreferenceUtil.exportPreferences(getShell(), getPreferenceStore());
            }
        });

        Button importButton = new Button(serializeGroup, SWT.PUSH);
        importButton.setText("Load...");
        importButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                PreferenceUtil.importPreferences(getShell(), getPreferenceStore());
                performApply();
                getShell().close();
            }
        });
    }

    private void createTabWidthControl(Composite parent) {
        Group container = new Group(parent, SWT.SHADOW_ETCHED_OUT);
        container.setText("General editor properties");
        GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_BEGINNING);
        container.setLayoutData(data);
        container.setLayout(new GridLayout(2, false));
        (new Label(container, SWT.NONE)).setText("Displayed tab width:");
        
        final Text input = new Text(container, SWT.BORDER | SWT.SINGLE);
        input.setText(String.valueOf(mTabWidth));
        data = new GridData();
        data.widthHint = 40;
        input.setLayoutData(data);
        
        input.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                try {
                    String text = input.getText();
                    int value = Integer.valueOf(text).intValue();
                    if (value >= MIN_TABWIDTH && value <= MAX_TABWIDTH) {
                        setErrorMessage(null);
                        setValid(true);
                        mTabWidth = value;
                    }
                    else {
                        setErrorMessage("Tab width shall be between " + MIN_TABWIDTH + " and " + MAX_TABWIDTH);
                        setValid(false);
                    }
                }
                catch (Exception exception)
                {
                    setValid(false);
                    setErrorMessage("Invalid tab width");
                }
            }
        });
    }
    
    public boolean performOk() {
        storeValues();
        return true;
    }

    public void performDefaults() {
        super.performDefaults();
        mTabWidth = DEFAULT_TABWIDTH;
        storeValues();
    }
    
    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(TAB_WIDTH, DEFAULT_TABWIDTH);
    }
    
    private void initializeValues() {
        IPreferenceStore store = getPreferenceStore();
        mTabWidth = store.getInt(TAB_WIDTH);
    }
    
    private void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        store.setValue(TAB_WIDTH, mTabWidth);
    }
    
    public static int getTabWidth() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore().getInt(TAB_WIDTH);
    }

}