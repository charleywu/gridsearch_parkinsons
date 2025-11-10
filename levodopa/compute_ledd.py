import pandas as pd
import numpy as np
from scipy.stats import ttest_ind

# Compute Levodopa Daily Dose (LDD) and Levopdopa Equivalency Daily Dose (LEDD)

# Based on https://doi.org/10.1002/mds.29410
# Jost ST, Kaldenbach M-A, Antonini A, Martinez-Martin P, Timmermann L, Odin P, et al. (2023)
# Levodopa Dose Equivalency in Parkinson’s Disease: Updated Systematic Review and Proposals.
# Movement Disorders: Official Journal of the Movement Disorder Society 38: 1236–1252.


mapping = {
    'Xadago': 'xadago_safinamide',
    'Levodopa/Benserazid (Madopar, Isicom,': 'levodopa_benserazid',
    'Levodopa/Carbidopa (Levo-Carb, LCE, ': 'levodopa_carbidopa',
    'Levodopa/Carbidopa/Entacapon (Levo-C-E, Stalevo': 'levodopa_carbidopa_entacapon',
    # 'Imbrija': 'levodopa_inhaled', # on demand / "bei Bedarf"
    'Rotigotin (Neupro Pflaster)': 'rotigotin_dopamine_agonist_patch',
    'Pramipexol (Sifrol, Oprymea)': 'pramipexol_dopamine_agonist',
    # 'Amantadin': 'amantadin'  # minor dopamingergic effect according to Tomlinson et al (2010)
    'Ongentys': 'ongentys_comt_inhibitor',
    'Rasagilin': 'rasagilin_maob_inhibitor',
    'Trivastal': 'trivastal_dopamine_agonist'
}

df = pd.read_excel('Medikamentenliste.xlsx')

subjects = sorted(set([int(s.split('(')[1][:-1]) if isinstance(s, str) else s for s in df.Pseudonym]))
dfout = pd.DataFrame(index=range(len(subjects)), columns=('id', 'condition',  'LDD', 'LEDD'))
dfout['id'] = subjects
for subject in df.Pseudonym:
    subject_id = int(subject.split('(')[1][:-1]) if isinstance(subject, str) else subject
    # check for subject 256 who appears twice (correct age is 62)
    cnd = (df.Pseudonym == subject) & (df.Alter == 62) if subject_id == 256 else (df.Pseudonym == subject)
    dfout.loc[dfout.id == subject_id, 'condition'] = df[cnd].Gruppe.item().replace(' ', '') if df[cnd].condition.isna().item() or df[cnd].condition.item() == '.' else df[cnd].condition.item()

for i in range(59, 100):
    d = df.iloc[i]
    subject_id = int(d.Pseudonym.split('(')[1][:-1]) if isinstance(d.Pseudonym, str) else d.Pseudonym
    if d['Gruppe'] != 'PNP':
        valid_entries = {mapping[col]: d[col] for col in mapping.keys() if isinstance(d[col], str) and not d[col].strip() == 'NA'}
        ledd, ldd = 0, 0
        operations_string = ''
        for j, (medication, entry) in enumerate(valid_entries.items()):
            if not ('edarf' in entry and not '+' in entry):  # captures Bedarf & bedarf
                if entry.count('x') == 2:
                    if entry == '1x 100 3x 150 mg':  # Catch this special case separately
                        freq = 4
                        dose = 137.5
                    else:
                        raise ValueError(f'String x appears twice for medication {medication} in entry {entry}, but special case is not catched')
                elif 'x' in entry:
                    freq = int(entry.split('x')[0])
                    if 'mg' in entry:
                        try:
                            dose = int(entry.split('x')[1].split('mg')[0])
                        except ValueError as e:
                            if '-' in entry:
                                dose = (int(entry.split('-')[0][-1]) + int(entry.split('-')[1][0])) / 2
                            else:
                                raise e
                    elif ('1x2' in entry) and (medication == 'levodopa_carbidopa'):
                        dose = 250
                    elif medication == 'levodopa_carbidopa':
                        dose = 125
                    elif (medication == 'levodopa_benserazid') and '125' in entry:
                        dose = 125
                    elif medication == 'trivastal_dopamine_agonist':
                        dose = 50  # according to Martha Sterf by email (Oct 30 2025)
                    elif medication == 'rasagilin_maob_inhibitor':
                        dose = 1  # according to Martha Sterf by email (Oct 30 2025)
                    else:
                        raise ValueError(f'String x in entry "{entry}" for medication {medication}, but no mg dosage')
                elif 'abends' in entry:
                    freq = 1
                    dose = int(entry.split('mg')[0])
                elif '4,5' in entry:
                    freq = 4.5
                    dose = int(entry.split('mg')[0].split(' ')[-1])
                elif medication == 'pramipexol_dopamine_agonist':
                    freq = 1
                    dose = float(entry.split('ret')[0].split('mg')[0].replace(',', '.'))
                else:
                    raise ValueError(f'No String x in entry "{entry}" for medication {medication}')
                if medication == 'xadago_safinamide':
                    ledd += 150  # fixed amount irrespective of dosage
                    operations_string += f'\t\t{j + 1}) {medication}: add a fixed value of 150mg to LEDD (irrespective of dose)\n'
                elif medication in ('levodopa_benserazid', 'levodopa_carbidopa', 'trivastal_dopamine_agonist'):
                    ledd += freq * dose
                    ldd += freq * dose
                    operations_string += f'\t\t{j + 1}) {medication}: add {freq} x {dose}mg to LEDD\n'
                elif medication == 'levodopa_carbidopa_entacapon':
                    ledd += freq * dose * 1.33
                    ldd += freq * dose
                    operations_string += f'\t\t{j + 1}) {medication}: add {freq} x {dose}mg x 1.33 to LEDD\n'
                elif medication == 'rotigotin_dopamine_agonist_patch':
                    ledd += dose * freq * 30
                    operations_string += f'\t\t{j + 1}) {medication}: add {freq} x {dose}mg x 30 to LEDD\n'
                elif medication == 'rasagilin_maob_inhibitor':
                    ledd += dose * freq * 100
                    operations_string += f'\t\t{j + 1}) {medication}: add {freq} x {dose}mg x 100 to LEDD\n'
                elif medication == 'pramipexol_dopamine_agonist':
                    ledd += dose * freq * 100
                    operations_string += f'\t\t{j + 1}) {medication}: add {freq} x {dose}mg x 100 to LEDD\n'
                elif medication == 'ongentys_comt_inhibitor':
                    if ldd == 0:
                        raise ValueError(f'LD is 0, but {medication} is provided with entry {entry}')
                    else:
                        ledd += 0.5 * ldd  # fixed rule irrespective of dosage
                        operations_string += f'\t\t{j + 1}) {medication}: add 0.5 x (LD={ldd}) to LEDD\n'
                else:
                    raise ValueError(f'No rule for medication {medication} with entry {entry}')
            else:
                operations_string += f'\t\t{j + 1}) {medication}: -\n'

        dfout.loc[dfout.id == subject_id, 'LDD'] = ldd
        dfout.loc[dfout.id == subject_id, 'LEDD'] = ledd
        print(f'Pseudonym={d.Pseudonym}')
        print(f'\tLEDD={ledd}, LDD={ldd}')
        print(f'\tEntries in Excel table:')
        for j, (k, v) in enumerate(valid_entries.items()):
            print(f'\t\t{j + 1}) {k}: {v}')
        print(f'\tOperations:\n{operations_string}')

dfout = dfout.astype(dict(LDD=float, LEDD=float))
dfout.to_csv('../data/levodopa.csv')


ldd_off = dfout[~dfout.LDD.isna() & (dfout.condition == 'PPD-')].LDD.values
ldd_on = dfout[~dfout.LDD.isna() & (dfout.condition == 'PPD+')].LDD.values
ledd_off = dfout[~dfout.LEDD.isna() & (dfout.condition == 'PPD-')].LEDD.values
ledd_on = dfout[~dfout.LEDD.isna() & (dfout.condition == 'PPD+')].LEDD.values

print('\n---------------------------------------------')
print(f'Patients who gave consent to their patient records (N_off={len(ldd_on)}, N_on={len(ldd_off)}) were taking an average Levodopa '
      f'dose of {dfout.LDD.mean():.1f} ± {dfout.LDD.std():.1f} mg/day (mean ± SD). The average total Levodopa Equivalent Daily Dose '
      f'was {dfout.LEDD.mean():.1f} ± {dfout.LDD.std():.1f} mg/day (mean ± SD), calculated according to Jost et al. (2023).')

print('\n')
stats_ldd = ttest_ind(ldd_off, ldd_on)
print(f'[LDD] PD-: {np.mean(ldd_off):.1f} ± {np.std(ldd_off, ddof=1):.1f} mg/day;  PD+: {np.mean(ldd_on):.1f} ± {np.std(ldd_on, ddof=1):.1f} mg/day; t = {stats_ldd.statistic:.1f}, p={stats_ldd.pvalue:.3f}')
stats_ledd = ttest_ind(ledd_off, ledd_on)
print(f'[LEDD] PD-: {np.mean(ledd_off):.1f} ± {np.std(ledd_off, ddof=1):.1f} mg/day;  PD+: {np.mean(ledd_on):.1f} ± {np.std(ledd_on, ddof=1):.1f} mg/day; t = {stats_ledd.statistic:.1f}, p={stats_ledd.pvalue:.3f}')

print(f'\nReference: Jost ST, Kaldenbach M-A, Antonini A, Martinez-Martin P, Timmermann L, Odin P, et al. (2023) Levodopa Dose '
      f'Equivalency in Parkinson’s Disease: Updated Systematic Review and Proposals. Movement Disorders: Official Journal '
      f'of the Movement Disorder Society 38: 1236–1252. DOI: 10.1002/mds.29410')
