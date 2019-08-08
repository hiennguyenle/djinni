// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from parcelable.djinni

package com.dropbox.djinni.test;

import java.util.HashMap;
import java.util.HashSet;
import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;

public class ParcelableOptionalMap implements android.os.Parcelable {


    /*package*/ final HashMap<String, HashSet<String>> mOptionalSet;

    public ParcelableOptionalMap(
            @CheckForNull HashMap<String, HashSet<String>> optionalSet) {
        this.mOptionalSet = optionalSet;
    }

    @CheckForNull
    public HashMap<String, HashSet<String>> getOptionalSet() {
        return mOptionalSet;
    }

    @Override
    public String toString() {
        return "ParcelableOptionalMap{" +
                "mOptionalSet=" + mOptionalSet +
        "}";
    }


    public static final android.os.Parcelable.Creator<ParcelableOptionalMap> CREATOR
        = new android.os.Parcelable.Creator<ParcelableOptionalMap>() {
        @Override
        public ParcelableOptionalMap createFromParcel(android.os.Parcel in) {
            return new ParcelableOptionalMap(in);
        }

        @Override
        public ParcelableOptionalMap[] newArray(int size) {
            return new ParcelableOptionalMap[size];
        }
    };

    public ParcelableOptionalMap(android.os.Parcel in) {
        if (in.readByte() == 0) {
            this.mOptionalSet = null;
        } else {
            this.mOptionalSet = new HashMap<String, HashSet<String>>();
            in.readMap(this.mOptionalSet, getClass().getClassLoader());
        }
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(android.os.Parcel out, int flags) {
        if (this.mOptionalSet != null) {
            out.writeByte((byte)1);
            out.writeMap(this.mOptionalSet);
        } else {
            out.writeByte((byte)0);
        }
    }

}