" ===============================================================
"         File: TransferChinese.vim
"       Author: Xiangjiang Ma <maxiangjiang@yahoo.com>
"  Last Change: 05/30/2004 Sun
"  Description: To mimic what gu and gU do for ascii,
"               I defined two new maps, gS and gT, to transfer between
"               the simplified Chinese and the traditional Chinese.
"      Version: 0.9.0
"        Usage: This file should reside in the plugins directory,
"               to be automatically sourced. If not, you can manually
"               source this file using ':source TransferChinese.vim'.
"               -------------------------------------------------
"               [range]ChineseToggle  (toggle between Chinese)
"               [range]gS  (transfer from Traditional to Simplified)
"               [range]gT  (transfer from Simplified to Traditional)
"               -------------------------------------------------
"     Features: (a) "quick and dirty" way to transfer Chinese to Chinese
"               (b) 20% of efforts for solving 80% of problems
"               (c) 2172 Chinese pairs are used for one-to-one map
"               (d) cursor positions are kept after gS and gT
"               (e) [range] for visual mode is supported
" ===============================================================


" ===============================================================
"                        CONTENTS
" ===============================================================
" (1) Chinese Processing Backgrounds
" (2) VIM Logic Control
" (3) Chinese Unicode Data
" ===============================================================

" ===============================================================
"                   Chinese Processing Backgrounds
" ===============================================================
"  Assumptions:
"  (1) The utf-8 format is used both in memory and on disk
"      (a) the encoding is set to be utf-8
"      (b) the text file to be transferred is in utf-8 format
"  (2) One-to-One map is assumed between the Simplified and the Traditional
"      (a) 2172 simplified  Chinese characters in s:simplified_chinese
"      (a) 2172 traditional Chinese characters in s:traditional_chinese
"  (3) correct .vimrc settings, for example, on my W2K box
"      set enc=utf8
"      set gfn=Courier_New:h12:w7
"      set gfw=SimSun-18030,Arial_Unicode_MS
"      set fencs=ucs-bom,utf8,chinese,ansi
"    " set fencs=ucs-bom,utf8,taiwan,ansi   "for reading big5 files
" ---------------------------------------------------------------
"  Pitfalls and Complexities:
"   * from simplified Chinese to traditional Chinese
"     one-to-many relationship can be as high as 21%
"   * from the traditional Chinese the simplified Chinese
"     one-to-many relationship is around 3.5% statistically
" ---------------------------------------------------------------
"  Unicode CJK information:
"   * UNICODE 3.0 (1999) has 49,194 assigned code points
"   * CJK is between U+4E00 (19968) and U+9FA5 (40869)
"   * CJK covers all big5 and gbk
"   * big5 covers 13,053  Chinese characters
"   * gb2312 covers 6,763 Chinese characters
" ---------------------------------------------------------------
"  References:
"  http://www.unicode.org
"  http://www.kanji.org
"  http://www.mandarintools.com/javaconverter.html
"  http://www.macchiato.com/unicode/charts.html
"  http://www-106.ibm.com/developerworks/library/utfencodingforms
"  http://www.cjk.org/cjk/cjki/cjk_uni.htm
"  http://www.china-language.gov.cn
"  google for "Encode-HanConvert site:http://search.cpan.org"
"  google for "unicode group:comp.software.international"
" ===============================================================


" ===============================================================
"                       VIM Logic Control
" ===============================================================

" Quit if already loaded or when 'compatible' is set
if exists("loaded_transfer_chinese") || &cp
   finish
end
let loaded_transfer_chinese = 1

map gS  Hmt``mz:GoSimplifiedChinese<CR>`tzt`z
map gT  Hmt``mz:GoTraditionalChinese<CR>`tzt`z

com! -range=% GoSimplifiedChinese  <line1>,<line2>call GoSimplifiedChinese()
com! -range=% GoTraditionalChinese <line1>,<line2>call GoTraditionalChinese()
com! -range=% ChineseToggle        <line1>,<line2>call ChineseToggle()

" Simplified Chinese is set by default,
" therefore, :ChineseToggle will do gS, for the first time
"
" To make :ChineseToggle will do gT for the first time,
" define the following in the .vimrc file:
"   let g:TransferChinese_Simplified = 1
"
if !exists('g:TransferChinese_Simplified')
    let g:TransferChinese_Simplified = 0
endif

" Following functions will take care of range.
" Following functions will abort as soon as an error is detected.
"
function! ChineseToggle() range abort
  let g:TransferChinese_Simplified = g:TransferChinese_Simplified+1
  if(g:TransferChinese_Simplified%2)
    exe a:firstline.",".a:lastline.'GoSimplifiedChinese'
  else
    exe a:firstline.",".a:lastline.'GoTraditionalChinese'
  endif
endfunction

function! GoSimplifiedChinese() range abort
  let from = s:traditional_chinese
  let to = s:simplified_chinese
  exe a:firstline.",".a:lastline.'call Transfer(from, to)'
endfunction

function! GoTraditionalChinese() range abort
  let from = s:simplified_chinese
  let to = s:traditional_chinese
  exe a:firstline.",".a:lastline.'call Transfer(from, to)'
endfunction

function! Transfer(from, to) range abort
  let from = a:from
  let to = a:to
  let s:gdefault_save = &gdefault
  set gdefault
  exe a:firstline.",".a:lastline.'s/./\=s:OneToOneMap(submatch(0),from,to)'
  let &gdefault = s:gdefault_save
  unlet s:gdefault_save
endfunction

function! s:OneToOneMap(char, from, to)
  if char2nr(a:char) < 19968
      return a:char
  endif
  let i = match(a:from, a:char)
  let m = strpart(a:to, i, 3)
  return (i >= 0 ? m : a:char)
endfunction
" ===============================================================

" ===============================================================
"                       Chinese Unicode Data
" ===============================================================

" ---------------------------------------------------------------
let s:simplified_chinese='
\皑蔼碍爱肮袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕
\毙币痹闭边编贬变辩辫标鳖别瘪濒滨宾摈饼并拨钵铂驳补财采参蚕残惭惨
\灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘
\陈衬撑称惩诚骋迟驰耻齿炽冲宠畴踌筹绸橱厨锄雏础储触处传疮闯创锤纯
\绰辞词赐聪葱囱从丛凑蹿窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷
\导盗灯邓敌涤递缔颠点垫电淀钓调谍叠钉顶锭订丢东动栋冻犊独读赌镀锻
\断缎兑队对吨顿钝夺堕鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦贩饭访纺飞诽
\废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖赶秆赣
\冈刚钢纲岗镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规归
\龟闺轨诡柜贵刽辊滚锅国过骇韩汉号阂鹤贺横恒轰鸿红壶护沪户哗华画划
\话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑获货祸击机
\积饥迹讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间
\艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧将浆蒋桨奖讲酱胶
\浇骄娇搅铰矫侥脚饺缴绞轿较阶节洁结诫届紧锦仅谨进晋烬尽劲荆茎鲸惊
\经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢觉决诀绝钧军骏开凯颗壳课垦
\恳抠库裤块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览
\懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟敛
\脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃灵岭领馏刘龙聋咙笼垄拢
\陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱
\抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩
\猫锚铆贸么没镁门闷们锰梦眯谜弥觅幂绵缅庙灭悯闽鸣铭谬谋亩呐钠纳难
\挠脑恼闹馁内拟腻撵酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤
\盘庞抛赔喷鹏骗飘频贫苹凭评泼颇铺谱栖凄脐齐骑岂启气弃讫牵铅迁签谦
\钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲寝轻氢倾顷请庆琼穷趋区
\躯驱龋颧权劝却鹊确让饶扰绕热韧认纫荣绒软锐闰润萨鳃赛叁伞丧骚扫涩
\杀刹纱筛删闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗时蚀
\实识驶势适释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲松耸
\怂颂讼诵擞苏诉肃虽随绥岁孙损笋缩琐锁獭挞态摊贪瘫滩坛谭谈叹汤烫涛
\绦讨腾誊锑题体屉条贴铁厅听烃铜统头秃图团颓蜕脱鸵驮驼椭洼袜弯湾顽
\万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问挝蜗涡窝卧呜钨乌诬无芜吴坞
\雾务误锡牺袭习铣戏细虾辖峡侠狭厦吓鲜纤贤衔闲显险现献县馅羡宪线厢
\镶乡详响项萧嚣销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许叙绪续
\轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯
\杨扬疡阳养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪蚁艺亿忆义诣议谊译异
\绎荫阴银饮隐樱婴鹰应缨莹萤营荧蝇赢颖哟拥痈踊咏优忧邮铀犹诱于舆鱼
\渔娱与屿语狱誉预驭鸳渊辕园员圆缘远愿约跃钥粤悦阅云郧匀陨运蕴酝晕
\韵杂灾载攒暂赞赃脏凿枣责择则泽贼赠轧铡闸栅诈斋债毡盏斩辗崭栈占战
\绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰争帧郑证织职执只纸挚掷帜
\质滞钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸驻专砖转赚桩装妆壮状锥赘
\坠缀谆着浊兹资渍踪综总纵邹诅组钻亘睾芈啬厍厣靥赝匦匮赜刭刿剀伛伥
\伧伫侪侬俦俨俪俣偾偬偻傥傧傩氽佥籴黉冁凫兖衮亵脔禀冢讦讧讪讴讵讷
\诂诃诋诏诒诓诔诖诘诙诜诟诠诤诨诩诮诰诳诶诹诼诿谀谂谄谇谌谏谑谒谔
\谕谖谙谛谘谝谟谠谡谥谧谪谫谮谯谲谳谵谶卺陉陧邝邬邺郏郐郓郦刍奂劢
\巯垩圹坜垆垭垲埘埚埙芗苈苋苌苁苎茏茑茔茕荛荜荞荟荠荦荥荩荪荭莳莴
\莅莸莺萦蒇蒉蒌蓦蓠蓣蔹蔺蕲薮藓奁尴扪抟挢掴掼揿摅搌撄撷撸撺弑叽呒
\呓呖呗咛哒哓哔哕哙哜哝唛唠唢啧啭喽喾嗫嗳辔嘤噜囵帏帱帻帼岖岘岚峄
\峤峥崂崃嵘嵛嵝巅徕犷狯狲猃猡猕饧饨饩饪饫饬饴饷饽馀馄馊馍馐馑馔庑
\赓廪忏怃怄忾怅怆怿恸恹恻恺恽悭惬愠愦懔闩闫闱闳闵闶闼闾阃阄阆阈阊
\阌阍阏阒阕阖阗阙阚沣沩泷泸泺泾浃浈浍浏浒浔涞涠渎渑渖渌溆滟滠滢滗
\潆潇潋潴濑灏骞迩迳逦屦弪妩妪妫姗娅娆娈娲娴婵媪嫒嫔嫱嬷驵驷驸驺驿
\驽骀骁骅骈骊骐骒骓骖骘骛骜骝骟骠骢骣骥骧纡纣纥纨纩纭纰纾绀绁绂绉
\绋绌绗绛绠绡绨绫绮绯绲缍绶绺绻绾缁缂缃缇缈缋缌缏缑缒缗缙缜缛缟缡
\缢缣缤缥缦缧缪缫缬缭缯缱缲缳缵玑玮珏珑顼玺珲琏瑷璎瓒韪韫韬杩枥枨
\枞枭栉栊栌栀栎柽桠桡桢桤桦桧栾棂椟椠椤椁榄榇榈榉槟槠樯橥橹橼檩殁
\殇殒殓殚殡轫轭轲轳轵轶轸轹轺轼轾辁辂辄辇辋辍辎辏辘辚戋戗戬瓯昙晔
\晖暧贲贳贶贻贽赀赅赆赈赉赇赕赙觇觊觋觌觎觏觐觑毵氇氩氲牍胧胪胄胫
\脍脶腽膑欤飑飒飓飕飙毂齑斓炀炜炖烨焖焘祢祯禅怼悫懑戆泶矶砀砗砺砻
\硖硗碛碜礴龛睐睑畲罴羁钆钇钋钊钌钍钏钐钔钗钕钛钜钣钤钫钪钭钬钯钰
\钲钴钶钸钹钺钼钽钿铄铈铉铊铋铌铍铎铐铑铒铕铖铗铙铛铟铠铢铤铥铧铨
\铪铩铫铮铯铳铴铵铷铹铼铽铿锂锆锇锉锊锒锓锔锕锖锛锞锟锢锩锬锱锲锴
\锶锷锸锼锾镂锵镆镉镌镏镒镓镔镖镗镘镙镛镞镟镝镡镤镦镧镨镪镫镬镯镱
\镳锺穑鸠鸢鸨鸩鸪鸫鸬鸲鸱鸶鸸鸷鸹鸺鸾鹁鹂鹄鹆鹇鹈鹉鹌鹎鹑鹕鹗鹞鹣
\鹦鹧鹨鹩鹪鹫鹬鹭鹳疖疠痨痫瘅瘗瘿瘾癞癫窦窭裆裢裣裥褛褴皲耧聍聩顸
\颀颃颉颌颏颔颚颛颞颟颡颢颦虬虮虿蚬蛎蛏蛱蛲蛳蛴蝈蝾蝼蠼罂笃笕笾筚
\筝箦箧箨箪箫篑簖籁舣舻袅羟糁絷麸趱酽酾鹾趸跄跞跷跸跹跻踬踯蹑蹒蹰
\躏躜觞觯靓雳霁霭龀龃龅龆龇龈龉龊龌黾鼋鼍隽雠銮錾鱿鲂鲅鲈稣鲋鲎鲐
\鲒鲔鲕鲚鲛鲞鲟鲠鲡鲢鲣鲥鲦鲧鲨鲩鲫鲭鲮鲰鲱鲲鲳鲵鲶鲷鲻鲽鳄鳅鳆鳇
\鳌鳍鳎鳏鳐鳓鳔鳕鳗鳜鳝鳟鳢鞑鞯鹘髅髋髌魇魉飨餍鬓麽黩黪鼹'
" ---------------------------------------------------------------

let s:traditional_chinese='
\皚藹礙愛骯襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢
\斃幣痺閉邊編貶變辯辮標鱉別癟瀕濱賓擯餅並撥缽鉑駁補財採參蠶殘慚慘
\燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟產闡顫場嘗長償腸廠暢鈔車徹塵
\陳襯撐稱懲誠騁遲馳恥齒熾沖寵疇躊籌綢櫥廚鋤雛礎儲觸處傳瘡闖創錘純
\綽辭詞賜聰蔥囪從叢湊躥竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱
\導盜燈鄧敵滌遞締顛點墊電澱釣調諜疊釘頂錠訂丟東動棟凍犢獨讀賭鍍鍛
\斷緞兌隊對噸頓鈍奪墮鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩販飯訪紡飛誹
\廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦復負訃婦縛該鈣蓋趕稈贛
\岡剛鋼綱崗鎬擱鴿閣鉻個給龔宮鞏貢鉤溝構購夠蠱顧剮關觀館慣貫廣規歸
\龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢號閡鶴賀橫恆轟鴻紅壺護滬戶嘩華畫劃
\話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴匯諱誨繪葷渾獲貨禍擊機
\積飢跡譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間
\艱緘繭檢鹼鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗將漿蔣槳獎講醬膠
\澆驕嬌攪鉸矯僥腳餃繳絞轎較階節潔結誡屆緊錦僅謹進晉燼盡勁荊莖鯨驚
\經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹覺決訣絕鈞軍駿開凱顆殼課墾
\懇摳庫褲塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽
\懶纜爛濫撈勞澇樂鐳壘類淚籬離裡鯉禮麗厲勵礫歷瀝隸倆聯蓮連鐮憐漣斂
\臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴靈嶺領餾劉龍聾嚨籠壟攏
\隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂
\掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾
\貓錨鉚貿麼沒鎂門悶們錳夢瞇謎彌覓冪綿緬廟滅憫閩鳴銘謬謀畝吶鈉納難
\撓腦惱鬧餒內擬膩攆釀鳥聶嚙鑷鎳檸獰寧擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚
\盤龐拋賠噴鵬騙飄頻貧蘋憑評潑頗鋪譜棲淒臍齊騎豈啟氣棄訖牽鉛遷簽謙
\錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親寢輕氫傾頃請慶瓊窮趨區
\軀驅齲顴權勸卻鵲確讓饒擾繞熱韌認紉榮絨軟銳閏潤薩鰓賽三傘喪騷掃澀
\殺剎紗篩刪閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩時蝕
\實識駛勢適釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼鬆聳
\慫頌訟誦擻蘇訴肅雖隨綏歲孫損筍縮瑣鎖獺撻態攤貪癱灘壇譚談嘆湯燙濤
\絛討騰謄銻題體屜條貼鐵廳聽烴銅統頭禿圖團頹蛻脫鴕馱駝橢窪襪彎灣頑
\萬網韋違圍為濰維葦偉偽緯謂衛溫聞紋穩問撾蝸渦窩臥嗚鎢烏誣無蕪吳塢
\霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈嚇鮮纖賢銜閑顯險現獻縣餡羨憲線廂
\鑲鄉詳響項蕭囂銷曉嘯蠍協挾攜脅諧寫瀉謝鋅舋興洶鏽繡虛噓須許敘緒續
\軒懸選癬絢學勛詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顏閻艷厭硯彥諺驗鴦
\楊揚瘍陽養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀蟻藝億憶義詣議誼譯異
\繹蔭陰銀飲隱櫻嬰鷹應纓瑩螢營熒蠅贏穎喲擁癰踴詠優憂郵鈾猶誘於輿魚
\漁娛與嶼語獄譽預馭鴛淵轅園員圓緣遠願約躍鑰粵悅閱雲鄖勻隕運蘊醞暈
\韻雜災載攢暫贊贓臟鑿棗責擇則澤賊贈軋鍘閘柵詐齋債氈盞斬輾嶄棧佔戰
\綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙爭幀鄭証織職執隻紙摯擲幟
\質滯鐘終種腫眾謅軸皺晝驟豬諸誅燭矚囑貯鑄駐專磚轉賺樁裝妝壯狀錐贅
\墜綴諄著濁茲資漬蹤綜總縱鄒詛組鑽亙睪羋嗇厙厴靨贗匭匱賾剄劌剴傴倀
\傖佇儕儂儔儼儷俁僨傯僂儻儐儺汆僉糴黌囅鳧兗袞褻臠稟塚訐訌訕謳詎訥
\詁訶詆詔詒誆誄詿詰詼詵詬詮諍諢詡誚誥誑誒諏諑諉諛諗諂誶諶諫謔謁諤
\諭諼諳諦諮諞謨讜謖謚謐謫譾譖譙譎讞譫讖巹陘隉鄺鄔鄴郟鄶鄆酈芻奐勱
\胇堊壙壢壚埡塏塒堝塤薌藶莧萇蓯苧蘢蔦塋煢蕘蓽蕎薈薺犖滎藎蓀葒蒔萵
\蒞蕕鶯縈蕆蕢蔞驀蘺蕷蘞藺蘄藪蘚奩尷捫摶撟摑摜撳攄榐攖擷擼攛弒嘰嘸
\囈嚦唄嚀噠嘵嗶噦噲嚌噥嘜嘮嗩嘖囀嘍嚳囁噯轡嚶嚕圇幃幬幘幗嶇峴嵐嶧
\嶠崢嶗崍嶸崳嶁巔徠獷獪猻獫玀獼餳飩餼飪飫飭飴餉餑餘餛餿饃饈饉饌廡
\賡廩懺憮慪愾悵愴懌慟懨惻愷惲慳愜慍憒懍閂閆闈閎閔閌闥閭閫鬮閬閾閶
\閿閽閼闃闋闔闐闕闞灃溈瀧瀘濼涇浹湞澮瀏滸潯淶潿瀆澠瀋淥漵灩灄瀅潷
\瀠瀟瀲瀦瀨灝騫邇逕邐屨弳嫵嫗媯姍婭嬈孌媧嫻嬋媼嬡嬪嬙嬤駔駟駙騶驛
\駑駘驍驊駢驪騏騍騅驂騭騖驁騮騸驃驄驏驥驤紆紂紇紈纊紜紕紓紺紲紱縐
\紼絀絎絳綆綃綈綾綺緋緄綞綬綹綣綰緇緙緗緹緲繢緦緶緱縋緡縉縝縟縞縭
\縊縑繽縹縵縲繆繅纈繚繒繾繰繯纘璣瑋玨瓏頊璽琿璉璦瓔瓚韙韞韜榪櫪棖
\樅梟櫛櫳櫨梔櫟檉椏橈楨榿樺檜欒櫺櫝槧欏槨欖櫬櫚櫸檳櫧檣櫫櫓櫞檁歿
\殤殞殮殫殯軔軛軻轤軹軼軫轢軺軾輊輇輅輒輦輞輟輜輳轆轔戔戧戩甌曇曄
\暉曖賁貰貺貽贄貲賅贐賑賚賕賧賻覘覬覡覿覦覯覲覷毿氌氬氳牘朧臚冑脛
\膾腡膃臏歟颮颯颶颼飆轂齏斕煬煒燉燁燜燾檷禎禪懟愨懣戇澩磯碭硨礪礱
\硤磽磧磣礡龕睞瞼畬羆羈釓釔釙釗釕釷釧釤鍆釵釹鈦鉅鈑鈐鈁鈧鈄鈥鈀鈺
\鉦鈷鈳鈽鈸鉞鉬鉭鈿鑠鈰鉉鉈鉍鈮鈹鐸銬銠鉺銪鋮鋏鐃鐺銦鎧銖鋌銩鏵銓
\鉿鎩銚錚銫銃鐋銨銣鐒錸鋱鏗鋰鋯鋨銼鋝鋃鋟鋦錒錆錛錁錕錮錈錟錙鍥鍇
\鍶鍔鍤鎪鍰鏤鏘鏌鎘鐫鎦鎰鎵鑌鏢鏜鏝鏍鏞鏃鏇鏑鐔鏷鐓鑭鐠鏹鐙鑊鐲鐿
\鑣鍾穡鳩鳶鴇鴆鴣鶇鸕鴝鴟鷥鴯鷙鴰鵂鸞鵓鸝鵠鵒鷴鵜鵡鵪鵯鶉鶘鶚鷂鶼
\鸚鷓鷚鷯鷦鷲鷸鷺鸛癤癘癆癇癉瘞癭癮癩癲竇窶襠褳襝襉褸襤皸耬聹聵頇
\頎頏頡頜頦頷顎顓顳顢顙顥顰虯蟣蠆蜆蠣蟶蛺蟯螄蠐蟈蠑螻蠷罌篤筧籩篳
\箏簀篋籜簞簫簣籪籟艤艫裊羥糝縶麩趲釅釃鹺躉蹌躒蹺蹕躚躋躓躑躡蹣躕
\躪躦觴觶靚靂霽靄齔齟齙齠齜齦齬齪齷黽黿鼉雋讎鑾鏨魷魴鱍鱸穌鮒鱟鮐
\鮚鮪鮞鱭鮫鯗鱘鯁鱺鰱鰹鰣鰷鯀鯊鯇鯽鯖鯪鯫鯡鯤鯧鯢鯰鯛鯔鰈鱷鰍鰒鰉
\鰲鰭鰨鰥鰩鰳鰾鱈鰻鱖鱔鱒鱧韃韉鶻髏髖髕魘魎饗饜鬢麼黷黲鼴'
" ===============================================================

