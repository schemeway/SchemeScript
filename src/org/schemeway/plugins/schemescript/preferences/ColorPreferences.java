/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.util.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.editor.*;

public class ColorPreferences extends SchemePreferencePage {

    static class AppearanceItem {
        public String name;
        public String label;
        public RGB color;
        public boolean bold;
        public boolean italic;
        public RGB defaultColor;
        public boolean defaultBold;
        public boolean defaultItalic;

        public AppearanceItem(String itemName, String itemLabel, RGB col, boolean isBold, boolean isItalic) {
            name = itemName;
            label = itemLabel;
            defaultColor = color = col;
            defaultBold = bold = isBold;
            defaultItalic = italic = isItalic;
        }
    }

    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".colors.";
    public final static String BACKGROUND_COLOR = PREFIX + "background";
    public final static String SYSTEM_BACKGROUND = PREFIX + "systembackground";
    public final static String MATCHER_COLOR = PREFIX + "matcher";
    public final static String MATCHER_BOX = PREFIX + "matcherbox";

    public final static String DEFAULT_COLOR = PREFIX + "default";
    public final static String PAREN_COLOR = PREFIX + "parenthesis";
    public final static String COMMENT_COLOR = PREFIX + "comment";
    public final static String DEFINE_COLOR = PREFIX + "define";
    public final static String KEYWORD_COLOR = PREFIX + "keyword";
    public final static String KEY_COLOR = PREFIX + "key";
    public final static String SPECIAL_COLOR = PREFIX + "special";
    public final static String ERROR_COLOR = PREFIX + "error";
    public final static String STRING_COLOR = PREFIX + "string";
    public final static String CONSTANT_COLOR = PREFIX + "constant";
    public final static String MUTATOR_COLOR = PREFIX + "mutator";
    public final static String TYPE_COLOR = PREFIX + "type";

    private static AppearanceItem[] mItems =
    {
        new AppearanceItem(DEFAULT_COLOR, "Default color", ISchemeColorConstants.SCHEME_DEFAULT, false, false),
        new AppearanceItem(PAREN_COLOR, "Parentheses", ISchemeColorConstants.SCHEME_PAREN, false, false),
        new AppearanceItem(COMMENT_COLOR, "Comments", ISchemeColorConstants.SCHEME_COMMENT, false, false),
        new AppearanceItem(DEFINE_COLOR, "Defining forms", ISchemeColorConstants.SCHEME_DEFINE, true, false),
        new AppearanceItem(KEYWORD_COLOR, "Special named constants", ISchemeColorConstants.SCHEME_KEYWORD, true, false),
        new AppearanceItem(KEY_COLOR, "Keywords", ISchemeColorConstants.SCHEME_KEY, true, true),
        new AppearanceItem(SPECIAL_COLOR, "Special forms", ISchemeColorConstants.SCHEME_SPECIAL, true, false),
        new AppearanceItem(ERROR_COLOR, "Error", ISchemeColorConstants.SCHEME_ERROR, false, false),
        new AppearanceItem(STRING_COLOR, "Strings", ISchemeColorConstants.SCHEME_STRING, false, false),
        new AppearanceItem(CONSTANT_COLOR, "Constants", ISchemeColorConstants.SCHEME_CONSTANT, false, false),
        new AppearanceItem(MUTATOR_COLOR, "Mutators", ISchemeColorConstants.SCHEME_MUTATOR, true, false),
        new AppearanceItem(TYPE_COLOR, "Type names", ISchemeColorConstants.SCHEME_TYPE, false, false)
    };

    private ColorSelector mCurrentItemColor;
    private Button mCurrentItemBold;
    private Button mCurrentItemItalic;

    private List mItemList;
    private ColorSelector mBackgroundColorSelector;
    private ColorSelector mMatchingColorSelector;
    private Button mMatchingBox;
    private Button mSystemBackground;

    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(1, false));

        createParenMatchingBox(composite);
        createBackgroundSection(composite);
        createForegroundSection(composite);

        initializeValues();
        return composite;
    }

    private void createParenMatchingBox(Composite parent) {
        Group group = new Group(parent, SWT.NONE);
        group.setText("Matched parenthesis");
        group.setLayout(new GridLayout(2, false));
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        new Label(group, SWT.NONE).setText("Matched parenthesis color: ");
        mMatchingColorSelector = new ColorSelector(group);
        new Label(group, SWT.NONE).setText("Draw box only: ");
        mMatchingBox = new Button(group, SWT.CHECK);
    }

    private void createBackgroundSection(Composite parent) {
        Group group = new Group(parent, SWT.NONE);
        group.setText("Background");
        group.setLayout(new GridLayout(2, false));
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        new Label(group, SWT.NONE).setText("Use system default: ");
        mSystemBackground = new Button(group, SWT.CHECK);
        new Label(group, SWT.NONE).setText("Background color: ");
        mBackgroundColorSelector = new ColorSelector(group);
        
        mSystemBackground.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e)
            {
                mBackgroundColorSelector.setEnabled(!(mSystemBackground.getSelection()));
            } 
        });
    }

    private void createForegroundSection(Composite parent) {
        Group group = new Group(parent, SWT.NONE);
        group.setText("Foreground");
        group.setLayout(new GridLayout(2, false));
        group.setLayoutData(new GridData(GridData.FILL_BOTH));

        mItemList = new List(group, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        mItemList.setLayoutData(new GridData(GridData.FILL_BOTH));
        Composite attributes = new Composite(group, SWT.NONE);
        attributes.setLayout(new GridLayout(2, false));

        new Label(attributes, SWT.NONE).setText("Color:");
        mCurrentItemColor = new ColorSelector(attributes);
        new Label(attributes, SWT.NONE).setText("Bold:");
        mCurrentItemBold = new Button(attributes, SWT.CHECK);
        new Label(attributes, SWT.NONE).setText("Italic:");
        mCurrentItemItalic = new Button(attributes, SWT.CHECK);

        String[] itemLabels = new String[mItems.length];
        for (int i = 0; i < itemLabels.length; i++) {
            itemLabels[i] = mItems[i].label;
        }
        mItemList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int index = mItemList.getSelectionIndex();
                if (index == -1)
                    return;
                AppearanceItem selectedItem = mItems[index];
                mCurrentItemColor.setColorValue(selectedItem.color);
                mCurrentItemBold.setSelection(selectedItem.bold);
                mCurrentItemItalic.setSelection(selectedItem.italic);
            }
        });
        mItemList.setItems(itemLabels);
        mItemList.select(0);

        mCurrentItemColor.setColorValue(mItems[0].color);
        mCurrentItemColor.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                int index = mItemList.getSelectionIndex();
                if (index == -1)
                    return;
                AppearanceItem selectedItem = mItems[index];
                selectedItem.color = mCurrentItemColor.getColorValue();
            }
        });
        
        mCurrentItemBold.setSelection(mItems[0].bold);
        mCurrentItemBold.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int index = mItemList.getSelectionIndex();
                if (index == -1)
                    return;
                AppearanceItem selectedItem = mItems[index];
                selectedItem.bold = mCurrentItemBold.getSelection();
            }
        });

        mCurrentItemItalic.setSelection(mItems[0].italic);
        mCurrentItemItalic.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int index = mItemList.getSelectionIndex();
                if (index == -1)
                    return;
                AppearanceItem selectedItem = mItems[index];
                selectedItem.italic = mCurrentItemItalic.getSelection();
            }
        });
    }

    public static void initializeDefaults(IPreferenceStore store) {
        PreferenceConverter.setDefault(store, MATCHER_COLOR, ISchemeColorConstants.MATCHING_PARENS);
        PreferenceConverter.setDefault(store, BACKGROUND_COLOR, ISchemeColorConstants.SCHEME_BACKGROUND);
        store.setDefault(MATCHER_BOX, false);
        store.setDefault(SYSTEM_BACKGROUND, true);

        for (int i = 0; i < mItems.length; i++) {
            AppearanceItem item = mItems[i];
            PreferenceConverter.setDefault(store, item.name, item.defaultColor);
            store.setDefault(boldAttribute(item), item.defaultBold);
            store.setDefault(italicAttribute(item), item.defaultItalic);
        }
    }

    protected void doPerformDefaults() {
        mBackgroundColorSelector.setColorValue(ISchemeColorConstants.SCHEME_BACKGROUND);
        mMatchingColorSelector.setColorValue(ISchemeColorConstants.MATCHING_PARENS);
        mMatchingBox.setSelection(false);
        mSystemBackground.setSelection(true);

        for (int i = 0; i < mItems.length; i++) {
            AppearanceItem item = mItems[i];
            item.color = item.defaultColor;
            item.bold = item.defaultBold;
            item.italic = item.defaultItalic;
        }
    }

    protected void initializeValues() {
        IPreferenceStore store = getPreferenceStore();
        mBackgroundColorSelector.setColorValue(PreferenceConverter.getColor(store, BACKGROUND_COLOR));
        mMatchingColorSelector.setColorValue(PreferenceConverter.getColor(store, MATCHER_COLOR));
        mMatchingBox.setSelection(store.getBoolean(MATCHER_BOX));
        mSystemBackground.setSelection(store.getBoolean(SYSTEM_BACKGROUND));
        mBackgroundColorSelector.setEnabled(!(mSystemBackground.getSelection()));

        for (int i = 0; i < mItems.length; i++) {
            AppearanceItem item = mItems[i];
            item.color = PreferenceConverter.getColor(store, item.name);
            item.bold = store.getBoolean(boldAttribute(item));
            item.italic = store.getBoolean(italicAttribute(item));
        }
    }

    protected void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        PreferenceConverter.setValue(store, MATCHER_COLOR, mMatchingColorSelector.getColorValue());
        PreferenceConverter.setValue(store, BACKGROUND_COLOR, mBackgroundColorSelector.getColorValue());
        store.setValue(MATCHER_BOX, mMatchingBox.getSelection());
        store.setValue(SYSTEM_BACKGROUND, mSystemBackground.getSelection());

        for (int i = 0; i < mItems.length; i++) {
            AppearanceItem item = mItems[i];
            PreferenceConverter.setValue(store, item.name, item.color);
            store.setValue(boldAttribute(item), item.bold);
            store.setValue(italicAttribute(item), item.italic);
        }
    }

    private static final String italicAttribute(AppearanceItem item) {
        return item.name + ".italic";
    }

    private static final String boldAttribute(AppearanceItem item) {
        return item.name + ".bold";
    }
}