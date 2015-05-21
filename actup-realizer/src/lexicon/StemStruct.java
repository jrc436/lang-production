package lexicon;

//I do not understand exactly how the _stems piece works, but this is a better solution to making it just an object
public class StemStruct {
	public final EntriesItem[] eItems;
	public final DataItem dItem;
	public StemStruct(EntriesItem[] eItems, DataItem dItem) {
		this.eItems = eItems;
		this.dItem = dItem;
	}
	public StemStruct(EntriesItem eItem) {
		this.eItems = new EntriesItem[] { eItem };
		this.dItem = null;
	}
}
