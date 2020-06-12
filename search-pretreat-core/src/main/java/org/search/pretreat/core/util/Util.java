package org.search.pretreat.core.util;

import net.sourceforge.pinyin4j.PinyinHelper;
import net.sourceforge.pinyin4j.format.HanyuPinyinCaseType;
import net.sourceforge.pinyin4j.format.HanyuPinyinOutputFormat;
import net.sourceforge.pinyin4j.format.HanyuPinyinToneType;
import net.sourceforge.pinyin4j.format.HanyuPinyinVCharType;

public class Util {
	private static HanyuPinyinOutputFormat hanyuPinyinOutputFormat;
	static {
		hanyuPinyinOutputFormat = new HanyuPinyinOutputFormat();
		hanyuPinyinOutputFormat.setCaseType(HanyuPinyinCaseType.LOWERCASE);
		hanyuPinyinOutputFormat.setToneType(HanyuPinyinToneType.WITHOUT_TONE);
		hanyuPinyinOutputFormat.setVCharType(HanyuPinyinVCharType.WITH_V);
	}

	public static String hanZiToPinYin(String query, boolean withWhiteSpace) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < query.length(); i++) {
			char ch = query.charAt(i);
			String[] tmp = null;
			try {
				tmp = PinyinHelper.toHanyuPinyinStringArray(ch, hanyuPinyinOutputFormat);
			} catch (Exception e) {
			}
			if (tmp == null) {
				sb.append(ch);
			} else {
				if (sb.length() > 0 && withWhiteSpace) {
					sb.append(" ");
				}
				sb.append(tmp[0]);
			}
		}
		return sb.toString().toLowerCase();
	}
}
