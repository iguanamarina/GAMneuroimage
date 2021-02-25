for i = 1:74

ppath_hdr = strcat('D:/Usuario/Desktop/ADvsCN/4.Analysis/AD/PET/',aa(i+2).name,'/**/wrP1.hdr');
ppath_img = strcat('D:/Usuario/Desktop/ADvsCN/4.Analysis/AD/PET/',aa(i+2).name,'/**/wrP1.img');

ppath_hdr = dir(ppath_hdr);
ppath_img = dir(ppath_img);

if length(ppath_hdr) > 0 %#ok<ISMT>
    
copyfile(strcat(ppath_hdr.folder,'/',ppath_hdr.name),strcat('D:/Usuario/Desktop/AD/',aa(i+2).name,'.hdr'));
copyfile(strcat(ppath_img.folder,'/',ppath_img.name),strcat('D:/Usuario/Desktop/AD/',aa(i+2).name,'.img'));

end

end
