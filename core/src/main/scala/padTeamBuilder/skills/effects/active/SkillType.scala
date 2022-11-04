package padTeamBuilder.skills.effects.active

enum SkillType {
  case AddCombosSkill, AllyDelay, AttributeChangeSelf,
    AwokenBindClear, BindClear, ChangeEnemyAttribute, ChangeTheWorld,
    ConditionalComponentHP, ConditionalEffect, CounterAttackSkill, DefenseBreak,
    Delay, EnhanceOrbs, EnhancedSkyfall, EvolvingEffect, Gravity, Haste, Heal,
    ImmediateDamage, IncreaseSkyfall, LeadSwap, LockOrbs, LockedSkyfall,
    MassAttack, MaxHPMult, MultiEffect, NailOrbSkyfall, NoEffect,
    NoSkyfallSkill, OrbChange, OrbTrace, Poison, RCVBoost, Random, Refresh,
    SelfUnmatchable, Shield, Spike, SpikeScaling, SpikeSlots, Spinner, Suicide,
    TimeExtend, Transform, UnableToUseSkills, UnlockOrbs, UnmatchableClear,
    Void, ConditionalComponentFloor, TimeReducedForTop2
}
