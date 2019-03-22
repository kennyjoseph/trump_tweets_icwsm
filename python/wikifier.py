import wikipedia
import wptools
from tqdm import tqdm
import io
import pandas as pd
def wikify(input_file):
    wiki_data = []
    fails = []

    to_wikify = [x for x in io.open(input_file)]
    for line in tqdm(to_wikify):
        try:
            wiki_data.append((line, wikipedia.page(line.strip())))
        except wikipedia.DisambiguationError:
            print 'cant disambiguate', line
            fails.append(line)
        except wikipedia.PageError:
            print 'cant find', line
            fails.append(line)


    img_all = []
    for x in tqdm(wiki_data):
        try:
            name_strs = [y.lower() for y in x[0].split() if len(y) > 2]
            imgs = []
            for img in x[1].images:
                for ns in name_strs:
                    if ns in img.lower():
                        imgs.append(img)
                        break
            if len(imgs) == 0:
                imgs = x[1].images

            img1 = imgs[0] if len(imgs) else ''
            img2 = imgs[1] if len(imgs) > 1 else ''
            img3 = imgs[2] if len(imgs) > 2 else ''
            img4 = imgs[3] if len(imgs) > 3 else ''
            img_all.append((x[0].strip(), img1,img2,img3,img4))
        except:
            continue

    infobox_data = []
    for name, wiki_pg in tqdm(wiki_data):
        print wiki_pg.title
        try:
            parse = wptools.page(wiki_pg.title).get_parse()
            infobox_data.append((name, parse))
        except LookupError:
            continue
        except UnicodeEncodeError:
            continue


    parsed_infobox_data = []
    infobox_fields = ['party','education', 'office', 'occupation',
                      'nationality', 'branch', 'allegiance', 'state', 'rank','image','state_house','known_for','office2']
    for name, u in infobox_data:
        dat = {"name" : name.strip()}
        if 'infobox' in u.data and u.data['infobox']:
            for x in infobox_fields:
                dat[x] = u.data['infobox'].get(x, '')
        else:
            for x in infobox_fields:
                dat[x] = ''
        parsed_infobox_data.append(dat)

    infoboxes = pd.DataFrame(parsed_infobox_data)
    imgs = pd.DataFrame(img_all, columns=['name','img1','img2','img3','img4'])
    all_wiki = pd.merge(infoboxes,imgs, on="name",how="outer")
    all_wiki.to_csv("../data/wiki_data.csv",encoding="utf8")
    return all_wiki


#wikify("../R/people_names.txt")