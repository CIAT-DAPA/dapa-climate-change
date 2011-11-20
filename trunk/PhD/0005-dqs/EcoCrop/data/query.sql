-- encabezado para el archivo
select 'id', 'data_provider_id', 'data_resource_id', 'institution_code_id',
'collection_code_id', 'caralogue_number_id', 'rank_name', 'specie_name',
'genus_name', 'family_name', 'order_name', 'class_name', 'phylum_name', 
'kingdom_name', 'specie_id', 'genus_id', 'family_id', 'order_id', 'class_id', 
'phylum_id', 'kingdom_id', 'rank_id', 'latitude', 'longitude', 'iso_country_code',
'year_created', 'month_created', 'occurrence_date', 'basis_of_record', 
'basis_of_record_name', 'altitude_metres'
union
-- todas las ocurrencias cuyo id del genus sea igual al id del registro anterior.
-- y además que las ocurrencias sean especies o cualquier sub-rango por debajo.
select occ2.id, occ2.data_provider_id, occ2.`data_resource_id`, 
occ2.`institution_code_id`, occ2.`collection_code_id`, occ2.`catalogue_number_id`,
-- rank name
(select r.name
from rank r
where tc2.rank = r.id) as "rank_name",
-- canonical species
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.species_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "specie_name",
-- canonical genus
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.genus_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "genus_name",
-- canonical family
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.family_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "family_name",
-- canonical order
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.order_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "order_name",
-- canonical class
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.class_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "class_name",
-- canonical phylum
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.phylum_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "phylum_name",
-- canonical kingdom
(select tn3.canonical 
from `taxon_name` tn3, `taxon_concept` tc3
where occ2.kingdom_concept_id = tc3.`id`
and tc3.`taxon_name_id` = tn3.id) as "kingdom_name",
occ2.`species_concept_id`, occ2.`genus_concept_id`, occ2.`family_concept_id`,
occ2.`order_concept_id`, occ2.`class_concept_id`, occ2.`phylum_concept_id`,
occ2.`kingdom_concept_id`, tc2.`rank`, occ2.`latitude`, occ2.`longitude`,
occ2.`iso_country_code`, occ2.`year`, occ2.`month`, occ2.`occurrence_date`,
occ2.`basis_of_record`,
(select br.br_value
from `lookup_basis_of_record` br
where occ2.basis_of_record = br.`br_key`), occ2.`altitude_metres`

from `occurrence_record` occ2, `taxon_concept` tc2, 
 (
       -- todos los conceptos taxonomicos que se llamen arachis y que 
    -- que sean generos.
       select tc.`id`, arachis_name_table.canonical
       from `taxon_concept` tc,
       (
       -- todos los taxon_names que se llamen Arachis y que sean generos.
             select tn.`id` as "arachis_name_id", tn.`canonical`
             from `taxon_name` tn
             where tn.`canonical` = "Arachis"
             and tn.`rank` = 6000
       ) arachis_name_table
       where tc.`taxon_name_id` = arachis_name_table.arachis_name_id
) arachis_taxon_concepts
where occ2.`genus_concept_id` = arachis_taxon_concepts.id
and occ2.`taxon_concept_id` = tc2.`id`
and tc2.`rank` >= 7000
and occ2.`geospatial_issue` = 0
and occ2.`basis_of_record` != 5
and occ2.`deleted` is null;
