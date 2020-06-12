package org.search.pretreat.core.commander;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.search.pretreat.core.model.response.INSTRUCTION_TYPE;
import org.search.pretreat.core.model.response.Instruction;
import org.search.pretreat.core.util.Util;

public class InstructionCommander {
    private static Set<String> searchShopOnlyCache = new HashSet<>();                // 只搜店铺名的词
	// 0.对照组；1~3.各种重改写策略；4.上位词策略；7.单字过滤pattern
	public static List<Instruction> getInstruction(String query, String cityId, double latitude, double longitude,
			String abInfo, boolean isFoodQuery) {
		List<Instruction> ret = new ArrayList<>();
//        if (query == null || prohibitedKeywordSet.contains(query)) {
//            return ret;
//        }
		// 如果原词就没有汉字，那直接传6；如果有同义词，再按同义词的来
		String hanZiToPinYin = Util.hanZiToPinYin(query, false);
		if (hanZiToPinYin.equals(query)) {
			Instruction instruction = new Instruction();
			instruction.setType(INSTRUCTION_TYPE.ORIGIN_KEYWORD);
			instruction.setValue(query);
		}

		return ret;
	}
}
