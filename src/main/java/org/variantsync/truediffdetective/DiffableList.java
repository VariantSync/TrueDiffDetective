package org.variantsync.truediffdetective;

import truechange.URI;
import truediff.Diffable;

public class DiffableList {
    protected static <T extends Diffable> truediff.DiffableList<T> withURI(truediff.DiffableList<T> dl, URI uri){
        return (truediff.DiffableList<T>)dl.withURI(uri);
    }
}
